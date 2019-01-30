(ql:quickload :ftw)

(defpackage moge
  (:use #:cl #:ftw #:cffi))

(in-package moge)

(defstruct blo ;;ブロック
  (x 0)
  (y 0)
  (x2 50)
  (y2 10)
  (color nil))

(defstruct (ball (:include blo))
  (angle 0)
  (vx 0)
  (vy 0)
  (speed 5)
  (r 0)
  (lastx 0)
  (lasty 0))


(defstruct (pad (:include blo))
  (speed 6)
  (blockhit 0)
  (padhit 0)
  (field nil)
  (ball nil)
  (addblock 0)
  (gameend 'win)
  (blocks nil))

(defstruct keystate
  (right nil)
  (left nil)
  (q nil))

(defparameter *screen-w* 960)
(defparameter *screen-h* 720)
(defparameter *waku-size* 10) ;;ゲームフィールドの周りの枠太さ
(defparameter *c-rect* nil) ;;クライアント領域
(defparameter *p* nil)
(defparameter *e* nil)
(defparameter *pe* nil)
(defparameter *keystate* (make-keystate))

(defparameter *field-w* 450)
(defparameter *field-h* 660)

(defparameter *pad-w* 50)
(defparameter *pad-h* 10)
(defparameter *block-w* 30)
(defparameter *block-h* 12)
(defparameter *block-num-x* 15) ;;初期xブロックの数
(defparameter *block-num-y* 15) ;;初期yブロックの数
(defparameter *blocks* nil)
(defparameter *brush* nil)
(defparameter *game-end* nil)
(defparameter *ball* nil)
(defparameter *start* t)
(defparameter *hmemDC* nil)
(defparameter *hbitmap* nil)
(defparameter *end-line* 500)
(defconstant +add-block-timer+ 2)
(defconstant +update-game+ 1)

;;四捨五入
(defun gonyu (n)
  (floor (+ 0.5 n)))

;;ブラシ生成
(defun set-brush ()
  (setf *brush* (make-array 7 :initial-contents
                              (list
                                (create-solid-brush (encode-rgb 128 0 255))
                                (create-solid-brush (encode-rgb 255 0 0))
                                (create-solid-brush (encode-rgb 0 255 0))
                                (create-solid-brush (encode-rgb 0 0 255))
                                (create-solid-brush (encode-rgb 255 255 0))
                                (create-solid-brush (encode-rgb 0 255 255))
                                (create-solid-brush (encode-rgb 255 0 255))))))

;;ブロック初期化
(defun init-blocks ()
  (loop for p in *pe*
        do (setf (pad-blocks p) nil)
           (loop for i from 0 below *block-num-y*
                 do (let ((brush (aref *brush* (mod i 7))))
                      (loop for j from 0 below *block-num-x*
                            do (when (>= (random 10) 3)
                                 (push (make-blo :x (+ (* j *block-w*) (blo-x (pad-field p)))
                                                 :y (+ (* i *block-h*) 80)
                                                 :x2 (+ (* j *block-w*) (blo-x (pad-field p)) *block-w*)
                                                 :y2 (+ (* i *block-h*) 80 *block-h*)
                                                 :color brush)
                                       (pad-blocks p))))))))

;;いろいろ初期化
(defun init-data (hwnd)
  (set-timer :hwnd hwnd :elapse 10 :replace-timer +update-game+)
  (set-timer :hwnd hwnd :elapse 20000 :replace-timer +add-block-timer+)
  (setf *c-rect* (get-client-rect hwnd)
        *p* (make-pad :x (- (+ 10 (floor *field-w* 2)) (floor *pad-w* 2))
                      :y (- (+ 10 *field-h*) 20) :speed 4)
        (pad-field *p*) (make-blo :x 10 :y 10 :x2 (+ *field-w* 10)
                                  :y2 (+ 10 *field-h*))
        *e* (make-pad :x (- (+ (blo-x2 (pad-field *p*)) (floor *field-w* 2)) (floor *pad-w* 2))
                      :y (- (+ 10 *field-h*) 20) :speed 5)
        (pad-field *e*) (make-blo :x (+ 20 (blo-x2 (pad-field *p*))) :y 10
                                  :x2 (+ 20 (blo-x2 (pad-field *p*)) *field-w*)
                                  :y2 (+ 10 *field-h*))
        (pad-x *p*) (floor (blo-x2 (pad-field *p*)) 2)
        (pad-y *p*) (- (blo-y2 (pad-field *p*)) 20)
        (pad-ball *p*) (make-ball :x (- (+ (pad-x *p*) (floor *pad-w* 2)) 8)
                                  :y (- (pad-y *p*) 18) :speed 3
                                  :x2 16 :y2 16 :r 8)
        (pad-ball *e*) (make-ball :x (- (+ (pad-x *e*) (floor *pad-w* 2)) 8)
                                  :y (- (pad-y *e*) 18) :speed 3
                                  :x2 16 :y2 16 :r 8)
        *pe* (list *p* *e*)))

;;ブラシ削除
(defun delete-brush ()
  (loop for i across *brush*
        do (delete-object i)))

;;キー押したとき
(defun moge-keydown (hwnd wparam)
  (let ((key (virtual-code-key wparam)))
    (case key
      (:left (setf (keystate-left *keystate*) t));;(decf (pad-x *p*) (pad-speed *p*)))
      (:right (setf (keystate-right *keystate*) t)) ;;(incf (pad-x *p*) (pad-speed *p*)))
      (:keyz
        (cond
          (*game-end*
            (setf *start* t
                  *game-end* nil)
            (init-data hwnd)
            (init-blocks)
            (invalidate-rect hwnd nil nil))
          (t
            (setf *start* nil)
            (dolist (p *pe*)
              (incf (ball-vx (pad-ball p)) (ball-speed (pad-ball p)))
              (incf (ball-vy (pad-ball p)) (- (ball-speed (pad-ball p))))))));; reset game

      (:keyq ;; quit
        (destroy-window hwnd)))))

;;キー話したとき
(defun moge-keyup (wparam)
  (let ((key (virtual-code-key wparam)))
    (case key
      (:left (setf (keystate-left *keystate*) nil));;(decf (pad-x *p*) (pad-speed *p*)))
      (:right (setf (keystate-right *keystate*) nil)))))
       ;;(incf (pad-x *p*) (pad-speed *p*)))

;;プレイヤーのパドル更新
(defun update-paddle-p ()
  (cond
    ((keystate-left *keystate*)
     (decf (pad-x *p*) (pad-speed *p*))
     (when (>= (blo-x (pad-field *p*)) (pad-x *p*))
       (setf (pad-x *p*) (blo-x (pad-field *p*))))
     (when *start*
       (setf (ball-x (pad-ball *p*)) (- (+ (pad-x *p*) (floor *pad-w* 2))
                                        (ball-r (pad-ball *p*))))))
    ((keystate-right *keystate*)
     (incf (pad-x *p*) (pad-speed *p*))
     (when (>= (+ (pad-x *p*) *pad-w*) (blo-x2 (pad-field *p*)))
       (setf (pad-x *p*) (- (blo-x2 (pad-field *p*)) *pad-w*)))
     (when *start*
       (setf (ball-x (pad-ball *p*)) (- (+ (pad-x *p*) (floor *pad-w* 2))
                                        (ball-r (pad-ball *p*))))))))

;;敵のパドル更新
(defun update-paddle-e ()
  (let* ((ball-px (+ (ball-x (pad-ball *e*)) (ball-r (pad-ball *e*))))
         (padcenter (+ (pad-x *e*) (floor *pad-w* 2))));;ボールの中心x
    (cond
      ((>= ball-px padcenter)
       (incf (pad-x *e*) (pad-speed *e*))
       (when (>= (+ (pad-x *e*) *pad-w*) (blo-x2 (pad-field *e*)))
         (setf (pad-x *e*) (- (blo-x2 (pad-field *e*)) *pad-w*))))
      ((< ball-px padcenter)
       (decf (pad-x *e*) (pad-speed *e*))
       (when (>= (blo-x (pad-field *e*)) (pad-x *e*))
         (setf (pad-x *e*) (blo-x (pad-field *e*))))))))

;;ボールの位置更新
(defun update-ball ()
  (when (null *start*)
    (dolist (p *pe*)
      (setf (ball-lastx (pad-ball p)) (ball-x (pad-ball p))
            (ball-lasty (pad-ball p)) (ball-y (pad-ball p)))
      (incf (ball-x (pad-ball p)) (gonyu (ball-vx (pad-ball p))))
      (incf (ball-y (pad-ball p)) (gonyu (ball-vy (pad-ball p)))))))

;;壁とボールの当たり判定
(defun hit-kabe? (p)
  (let* ((r (ball-r (pad-ball p)))
         (ball-px (+ (ball-x (pad-ball p)) r)) ;;ボールの中心x
         (ball-py (+ (ball-y (pad-ball p)) r)))
    (cond
      ((or (>= (+ (blo-x (pad-field p)) r) ball-px 0) ;;横壁
           (>= (+ *waku-size* (blo-x2 (pad-field p))) ball-px (- (blo-x2 (pad-field p)) r)))
       (setf (ball-vx (pad-ball p)) (- (ball-vx (pad-ball p)))))
      ((>= (+ (blo-y (pad-field p)) r) ball-py (blo-y (pad-field p))) ;;上壁
       (setf (ball-vy (pad-ball p)) (- (ball-vy (pad-ball p)))))
      ((>= (blo-y2 (pad-field p)) ball-py (- (blo-y2 (pad-field p)) r))
       (setf *game-end* t
             (pad-gameend p) 'lose))
      (t nil))))

;;ボールと壁の当たり判定チェック
(defun ball-hit-kabe (hwnd)
  (dolist (p *pe*)
    (let ((hoge (hit-kabe? p)))
      (cond
        ((eq hoge 'lose)
         (kill-timer +update-game+ hwnd)
         (kill-timer +add-block-timer+ hwnd))
        (hoge
          (play-sound "./wav/kabe-hit.wav" (list :filename :async)))))))
          ;;(setf (ball-x (pad-ball p)) (ball-lastx (pad-ball p))
          ;;      (ball-y (pad-ball p)) (ball-lasty (pad-ball p))))))))


;;ボールとプレイヤーパドルの当たり判定
(defun hit-paddle? (hwnd)
  (loop for p in *pe*
        for p2 in (list *e* *p*)
        do (let* ((r (ball-r (pad-ball p)))
                  (padx (pad-x p)) (pady (pad-y p))
                  (ball-px (+ (ball-x (pad-ball p)) r)) ;;ボールの中心x
                  (ball-py (+ (ball-y (pad-ball p)) r))
                  (y-r (- pady r)))
             (cond
               ((or (and (>= (+ padx *pad-w*) ball-px padx)
                         (>= pady ball-py y-r))
                    (>= r (sqrt (+ (expt (- ball-px padx) 2)
                                   (expt (- ball-py pady) 2))))
                    (>= r (sqrt (+ (expt (- ball-px (+ padx *pad-w*)) 2)
                                   (expt (- ball-py pady) 2)))))
                (let* ((pad-center (+ padx (floor *pad-w* 2)))
                       (dist (- ball-px pad-center))
                       (angle (- 90 (* (/ dist (/ *pad-w* 2)) 60)))
                       (rad (* angle (/ pi 180)))
                       (speed (sqrt (+ (expt (ball-vx (pad-ball p)) 2) (expt (ball-vy (pad-ball p)) 2)))))
                  (when (>= (pad-blockhit p) 2)
                    (dotimes (i (floor (pad-blockhit p) 2))
                      (add-block p2 hwnd)))
                  (incf (pad-padhit p))
                  (when (zerop (mod (pad-padhit p) 10))
                    (incf speed 0.2))
                  (when (= (gonyu (* (cos rad) speed)) 0)
                    (if (> (ball-vx (pad-ball p)) 0)
                        (setf rad (* 80 (/ pi 180)))
                        (setf rad (* 100 (/ pi 180)))))
                  (setf (ball-vx (pad-ball p)) (* (cos rad) speed)
                        (ball-vy (pad-ball p)) (* -1 (sin rad) speed)
                        (pad-blockhit p) 0)
                  (play-sound "./wav/pad-hit.wav" (list :filename :async))))))))

;;ボールとブロックの当たり判定
(defun hit-block? (p b)
  (let* ((r (ball-r (pad-ball p)))
         (ball-px (+ (ball-x (pad-ball p)) r)) ;;ボールの中心x
         (ball-py (+ (ball-y (pad-ball p)) r))
         (b1x (- (blo-x b) r)) (b2x (+ (blo-x2 b) r))
         (b1y (- (blo-y b) r)) (b2y (+ (blo-y2 b) r)))
    (cond
      ;;ブロックの横にあたった
      ((and (>= (blo-x2 b) ball-px (blo-x b))
            (>= b2y ball-py b1y))
       (setf (ball-vy (pad-ball p)) (- (ball-vy (pad-ball p)))))
      ;;ブロックの上下にあたった
      ((and (>= b2x ball-px b1x)
            (>= (blo-y2 b) ball-py (blo-y b)))
       (setf (ball-vx (pad-ball p)) (- (ball-vx (pad-ball p)))))
      ;;ブロックの斜めにあたった
      ((or (>= r (sqrt (+ (expt (- ball-px (blo-x b)) 2) (expt (- ball-py (blo-y b)) 2))))
           (>= r (sqrt (+ (expt (- ball-px (blo-x2 b)) 2) (expt (- ball-py (blo-y b)) 2))))
           (>= r (sqrt (+ (expt (- ball-px (blo-x b)) 2) (expt (- ball-py (blo-y2 b)) 2))))
           (>= r (sqrt (+ (expt (- ball-px (blo-x2 b)) 2) (expt (- ball-py (blo-y2 b)) 2)))))
       (setf (ball-vx (pad-ball p)) (- (ball-vx (pad-ball p)))
             (ball-vy (pad-ball p)) (- (ball-vy (pad-ball p)))))
      ;;当たらなかった
      (t nil))))

;;ボールとブロックの当たり判定チェック
(defun ball-hit-blocks? ()
  (dolist (p *pe*)
    (dolist (b (pad-blocks p))
      (let ((hoge (hit-block? p b)))
        (when hoge
          (play-sound "./wav/blo-hit.wav" (list :filename :async))
          (setf (pad-blocks p) (remove b (pad-blocks p) :test #'equal)
                (ball-x (pad-ball p)) (ball-lastx (pad-ball p))
                (ball-y (pad-ball p)) (ball-lasty (pad-ball p)))
          (incf (pad-blockhit p))
          (return))))))

;;ゲームフィールド描画
(defun render-field (hdc)
  (select-object hdc (get-stock-object :black-brush))
  (dolist (p *pe*)
    (rectangle hdc (blo-x (pad-field p)) (blo-y (pad-field p))
               (blo-x2 (pad-field p)) (blo-y2 (pad-field p)))))

;;パドル描画
(defun render-paddle (hdc)
  (select-object hdc (get-stock-object :white-brush))
  (dolist (p *pe*)
    (rectangle hdc (pad-x p) (pad-y p)
               (+ (pad-x p) (pad-x2 p))
               (+ (pad-y p) (pad-y2 p)))))


;;ブロック描画
(defun render-blocks (hdc)
  (dolist (p *pe*)
    (dolist (b (pad-blocks p))
      (select-object hdc (blo-color b))
      (rectangle hdc (blo-x b) (blo-y b) (blo-x2 b) (blo-y2 b)))))


;;ボール描画
(defun render-ball (hdc)
  (select-object hdc (get-stock-object :white-brush))
  (dolist (p *pe*)
    (ellipse hdc (ball-x (pad-ball p)) (ball-y (pad-ball p))
             (+ (ball-x (pad-ball p)) (ball-x2 (pad-ball p))) (+ (ball-y (pad-ball p)) (ball-y2 (pad-ball p))))))

;;バックグラウンド
(defun render-background (hdc)
  (select-object hdc (get-stock-object :white-brush))
  (rectangle hdc 0 0 (rect-right *c-rect*) (rect-bottom *c-rect*)))

;;この線をブロックが超えたら負け
(defun render-sen (hdc)
  (let ((pen (get-stock-object :white-pen)))
    (select-object hdc pen)
    (move-to hdc 10 *end-line*)
    (line-to hdc 960 *end-line*)
    (select-object hdc (get-stock-object :black-pen))))

;;ブロック追加
(defun add-block (p hwnd)
  (let ((brush (aref *brush* (random 7))))
    (dolist (b (pad-blocks p))
      (setf (blo-y b) (+ (blo-y b) *block-h*)
            (blo-y2 b) (+ (blo-y2 b) *block-h*))
      (when (and (null *game-end*) (>= (blo-y b) *end-line*))
        (setf *game-end* t
              (pad-gameend p) 'lose)
        (kill-timer +update-game+ hwnd)
        (kill-timer +add-block-timer+ hwnd)))
    (dotimes (j *block-num-x*)
      (when (>= (random 10) 3)
        (push (make-blo :x (+ (* j *block-w*) (blo-x (pad-field p)))
                        :y 80
                        :x2 (+ (* j *block-w*) (blo-x (pad-field p)) *block-w*)
                        :y2 (+ 80 *block-h*)
                        :color brush)
              (pad-blocks p))))))

;;勝敗メッセージ
(defun render-win-message (hdc)
  (let ((font (create-font "メイリオ" :height 140))
        (font2 (create-font "メイリオ" :height 40)))
    (select-object hdc font)
    (dolist (p *pe*)
      (let ((text (if (eq (pad-gameend p) 'win)
                      "勝ち" "負け")))
        (text-out hdc text (+ 130 (blo-x (pad-field p)))
                           (+ 220 (blo-y (pad-field p))))))
    (select-object hdc font2)
    (text-out hdc "z:もう一度" 130 400)
    (text-out hdc "q:終わる" 130 450)
    (select-object hdc (get-stock-object :system-font))
    (delete-object font)
    (delete-object font2)))

;;ゲーム画面描画
(defun render-game (hdc)
  (render-background *hmemdc*)
  (render-field *hmemdc*)
  (render-sen *hmemdc*)
  (render-blocks *hmemdc*)
  (render-ball *hmemdc*)
  (render-paddle *hmemdc*)
  (when *game-end*
    (render-win-message *hmemdc*))
  (bit-blt hdc 0 0 *hmemdc* 0 0 :width (rect-right *c-rect*)
           :height (rect-bottom *c-rect*) :raster-op :srccopy))


(defwndproc moge-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (set-brush)
     (init-data hwnd)
     (init-blocks)
     (with-dc (hdc hwnd)
       (setf *hmemdc* (create-compatible-dc hdc)
             *hbitmap* (create-compatible-bitmap hdc (rect-right *c-rect*) (rect-bottom *c-rect*)))
       (select-object *hmemdc* *hbitmap*)))
    ((const +wm-paint+)
     (with-paint (hwnd hdc)
       (render-game hdc)))
    ((const +wm-close+)
     (delete-dc *hmemdc*)
     (delete-object *hbitmap*)
     (destroy-window hwnd))
    ((const +wm-timer+)
     (when (= +add-block-timer+ wparam)
       (dolist (p *pe*)
         (add-block p hwnd)))
     (when (= +update-game+ wparam)
       (update-ball)
       (ball-hit-kabe hwnd)
       (ball-hit-blocks?)
       (hit-paddle? hwnd)
       (update-paddle-p)
       (update-paddle-e))

     (invalidate-rect hwnd nil nil))
    ((const +wm-keydown+)
     (moge-keydown hwnd wparam))
    ((const +wm-keyup+)
     (moge-keyup wparam))
    ((const +wm-destroy+)
     (delete-brush)
     (post-quit-message)))

  (default-window-proc hwnd msg wparam lparam))

(defun moge ()
  (register-class "MOGE" (callback moge-wndproc)
                  :cursor (load-cursor :arrow)
                  :background (get-stock-object :white-brush))
  (let ((hwnd (create-window "MOGE"
                             :window-name "HOGE"
                             :ex-styles (const +WS-EX-COMPOSITED+)
                             :styles (logior-consts +ws-overlappedwindow+ +ws-visible+)
                             :x 400 :y 100 :width 960 :height 720))
        (msg (make-msg)))
    (setf *start* t)
    (show-window hwnd)
    (update-window hwnd)
    (do ((done nil))
        (done)
      (let ((r (get-message msg)))
        (cond
          ((zerop r) (setf done t))
          (t
           (translate-message msg)
           (dispatch-message msg)))))))
