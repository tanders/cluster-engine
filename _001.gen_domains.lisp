(in-package :cluster-engine)

; pitch values  (& chords ?)
; (arithm-ser + mapcar list)
; including controls with chord-editors (range / modes-gammes façon Jacopo)

(defmethod pitchdomain-range ((min integer) (step integer) (max integer))
  (mapcar #'list (pw::arithm-ser min step max)))

(defmethod pitchdomain-range ((min list) (step integer) (max list))
  (pw::mat-trans (loop for i in min
        for j in max
    collect (pw::arithm-ser i step j))))

(defmethod pitchdomain-range-chords ((min list) (step integer) (max list))
  (mapcar #'list (pw::mat-trans (loop for i in min
                                  for j in max
                                  collect (pw::arithm-ser i step j)))))

; pitches motives (& chords ?)
; concatenation de motifs écrits (bach roll/score)

; pitches interval motives + start(s)
; concatenation de motifs écrits + x->dx (bach roll/score)

; chord intervals (solo or motives)
; mon système pour mémoire de l'eau

; rtm values (arithm-ser rescaled/squared + mapcar list)
; with rest or not

; rtm motives
; concatenation de motifs écrits (bach roll/score)
