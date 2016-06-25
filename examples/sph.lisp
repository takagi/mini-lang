

;;
;; SPH kernel functions

(ml:defun poly6-kernel (x)
  (let ((r (norm x)))
    (* (/ 315.0d0 (* 64.0d0 pi (pow h 0))) ; TODO: pow
       (pow (- (* h h) (* r r)) 3))))

(ml:defun grad-spiky-kernel (x)
  (let ((r (norm x)))
    (.* (/ -45.0d0 (* pi (pow h 6)))
        (pow (- h r) 2)
        (/. x r))))

(ml:defun rap-visc-kernel (x)
  (let ((r (norm x)))
    (* (/ 45.0d0 (* pi (pow h 6)))
       (- h r))))


;;
;; Update density


;;
;; Update pressure


;;
;; Update force

(ml:defun pressure-term (rho prs i j dr)
  (.* (/ (* (- pmass) (+ (aref prs i) (aref prs j))) ; TODO: negate
         (* 2.0d0 (aref rho j)))
      (grad-spiky-kernel dr)))

(ml:defun viscosity-term (vel rho i j dr)
  (*. (/. (.* visc pmass (- (aref vel j) (aref vel i)))
          (aref rho j))
      (rap-visc-kernel dr)))

(defun update-force (force pos vel rho prs n nbr)
  (loop repeat n
        for i from 0
     do (setf (double3-aref* force i)
              (eval-mini-lang
               (double3 0.0d0 0.0d0 0.0d0)))
        (do-neighbors (j nbr pos i)
          (when (/= i j)
            (setf (double3-aref* force i)
                  (eval-mini-lang
                   (let* ((dr (*. (- (aref pos i)
                                     (aref pos j))
                                  simscale)))
                       (if (<= (norm dr) h)
                           (+ (pressure-term rho prs i j dr)
                              (viscosity-term vel rho i j dr))
                           (double3 0.0d0 0.0d0 0.0d0))))))))))


;;
;; Update acceleration

(defun update-acceleration (acc force rho n)
  (loop repeat n
        for i from 0
     do (setf (double3-aref* acc i)
              (eval-mini-lang
               (+ (/. (aref force i)
                      (aref rho i))
                  (double3 0.0d0 -9.8d0 0.0d0))))))


;;
;; Update velocity

(defun update-velocity (vel acc n)
  (loop repeat n
        for i from 0
     do (setf (double3-aref* vel i)
              (eval-mini-lang
               (+ (aref vel i)
                  (*. (aref acc i) dt))))))


;;
;; Update position

(defun update-position (pos vel n)
  (loop repeat n
        for i from 0
     do (setf (double3-aref* pos i)
              (eval-mini-lang
               (+ (aref pos i)
                  (/. (*. (aref vel i) dt)
                      simscale))))))
