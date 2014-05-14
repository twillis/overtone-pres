;; 

(use 'overtone.core)
;; (boot-external-server)
;; (connect-external-server)

;; define: music 

;; vocal or instrumental sounds (or both) combined in
;; such a way as to produce beauty of form, harmony, and expression of
;; emotion.


;; or a series of sounds(notes) played in sequence


;; a note has pitch, volume/velocity, length

(defn mk-note [pitch volume length]
  {:pitch pitch :volume volume :length length})


(defn mk-note-seq [pitches volumes lengths play-ats]
  (map (fn [n p] {:note n :play-at p}) (map mk-note pitches volumes lengths) play-ats))


;; note + instrument = sound the FREE is important, it destroys the
;;synth node in SC after it's played, otherwise you will eventually
;;crash SC
(definst flute [frequency 440 volume 1 length 1000]
  (let [envelope (line volume 0 (/ length 1000) :action FREE)] 
    (* envelope (sin-osc frequency))))



;; issue: a synth takes a frequency but we have pitches so need to convert
(defn pitch->hz [pitch]
  (->> pitch
       note
       midi->hz))


;; issue: play-at and length need to be in ms but the data is in relative terms
;; to a metronome/tempo

(defn beat->ms [beat tempo]
  (tempo beat))

(defn duration->ms [duration tempo]
  (- (beat->ms duration tempo) (tempo 0)))




(comment

  (def yankee-doodle (mk-note-seq 
                      [:c6 :c6 :d6 :e6 :c6 :e6 :d6] 
                      [1 1 1 1 1 1 1] 
                      [1/2 1/2 1/2 1/2 1/2 1/2 1]
                      [0 1/2 1 3/2 2 5/2 3]))

  (def tempo (metronome 120))

  (doseq [{note :note play-at :play-at} yankee-doodle]
    (at (beat->ms play-at tempo)
        (flute (pitch->hz (:pitch note))
               (:volume note)
               (duration->ms (:length note) tempo))))
  )


(defn play-notes [notes inst tempo]
  (doseq [{note :note play-at :play-at} notes]
    (at (beat->ms play-at tempo)
        (inst (pitch->hz (:pitch note))
              (:volume note)
              (duration->ms (:length note) tempo)))))

;; so we have a crude representation for what I defined as music YAY!!!



;;;;;;;;;;;;;;;;;;
;; scaling up
;;;;;;;;;;;;;;;;;;

;; but what about chords? a series of notes played at the same time.

(comment
  (def a-chord (mk-note-seq [:e2 :b2 :e3 :g#3 :b3 :e4] ;; open e major power chord(guitar speak)
                            [1 1 1 1 1 1]
                            [4 4 4 4 4 4]
                            [0 0 0 0 0 0]))

  (play-notes a-chord flute (metronome 120))

  ;; or
  (def another-chord (mk-note-seq (concat (chord :c4 :major) (chord :eb4 :major))
                                  [1 1 1 1 1 1]
                                  [4 4 4 4 4 4]
                                  [0 0 0 0 0 0]))

  (play-notes another-chord flute (metronome 120))
  )

;; can mk-note-* be made to handle chords ??

(defn mk-note 
  "return collection of {:pitch :volume :length}"
  [pitch volume length]
  (let [pitches (if (not (coll? pitch)) [pitch] pitch)]
    (map (fn [p] {:pitch p :volume volume :length length}) pitches)))

;; refactor this
(defn mk-note-seq
  "return collection of {:note {:pitch :volume :length} :play-at}"
  [pitches volumes lengths play-ats]
  (let [intermediates (map (fn [a b] [a b]) 
                           (map mk-note pitches volumes lengths) play-ats)]
    (flatten 
     (map (fn [intermediate] 
            (map (fn [n] 
                   {:note n :play-at (last intermediate)}) (first intermediate))) 
          intermediates))))

(comment
  (def melody (mk-note-seq [(chord :c4 :major) :f4 
                            (chord :g3 :major) :f#3 
                            (chord :d4 :minor) :g4 
                            (chord :a3 :minor) :b3 
                            (chord :e4 :minor) :e3 
                            (chord :e4 :minor) :e3 
                            (chord :f4 :major) :f3 
                            (chord :f4 :major) :g3 
                            (chord :c4 :major)]
                           [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]
                           [4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 2 8]
                           [0 2 3 5 6 8 9 11 12 14 15 17 18 20 21 23 24 26]))

  (play-notes melody flute (metronome 120))
  (play-notes yankee-doodle flute (metronome 120))
  
  )

;; bars/patterns
;; {bar: 1 beats: 4} + tempo = offset(ms)
(defn mk-bar [pitches volumes lengths play-ats & {:keys [beats-per] :or {beats-per 4}}]
  {:beats-per beats-per :sequence (mk-note-seq pitches volumes lengths play-ats)})


;; assign bar given position in seq, how does beats per bar play into this?
(defn do-start-at [bars]
  (loop [ l (rest bars) acc [(assoc-in (first bars) [:start-at] 1)]]
    (if (empty? l)
      acc
      (recur (rest l) 
             (conj acc 
                   (assoc-in (first l) [:start-at] 
                             (+ (:start-at (last acc)) (:beats-per (first l)))))))))

(defn play-notes [notes inst tempo & {:keys [start-at] :or {start-at 1}}]
  (doseq [{note :note play-at :play-at} notes]
    (at (beat->ms (+ start-at play-at) tempo)
        (inst (pitch->hz (:pitch note))
              (:volume note)
              (duration->ms (:length note) tempo)))))

(defn play-bars [bars inst tempo & {:keys [start-at] :or {start-at 0}}]
  (let [the-bars (do-start-at bars)]
    (doseq [{bar :sequence bar-start-at :start-at} the-bars]
      (play-notes bar inst tempo :start-at (+ start-at bar-start-at)))))

(comment
  (def bass-e (mk-bar (take 8 (repeat [:e2 :e3]))
                      (take 8 (repeat 2))
                      (take 8 (repeat 1/2))
                      (take 8 (range 0 4 1/2))))

  (def bass-a (mk-bar (take 8 (repeat [:a2 :a3]))
                      (take 8 (repeat 2))
                      (take 8 (repeat 1/2))
                      (take 8 (range 0 4 1/2))))
  (def bass-a2 (mk-bar [[:a2 :a3 :d4] [:a2 :a3 :d4] [:a2 :a3 :d4] [:a2 :a3 ::c#4] [:a2 :a3 ::c#4] [:a2 :a3 ::c#4] [:a2 :a3 :d4] [:a2 :a3 :d4]]
                       (take 8 (repeat 2))
                       (take 8 (repeat 1/2))
                       (take 8 (range 0 4 1/2))))

  (def guitar-1 (mk-bar [[:e3 :e4 :e5] [:e3 :e4 :e5] [:e3 :e4 :e5] [:e3 :f#4 :f#5] [:e3 :f#4 :f#5] [:e3 :f#4 :f#5] [:e3 :e4 :e5] [:e3 :e4 :e5]]
                        (take 8 (repeat 3))
                        (take 8 (repeat 1/2))
                        (take 8 (range 0 4 1/2))
                        ))
  (def guitar-2 (mk-bar [[:e3 :g4 :g5] [:e3 :g4 :g5] [:e3 :g4 :g5] [:e3 :a4 :a5] [:e3 :a4 :a5] [:e3 :a4 :a5] [:e3 :g4 :g5] [:e3 :g4 :g5]]
                        (take 8 (repeat 3))
                        (take 8 (repeat 1/2))
                        (take 8 (range 0 4 1/2))
                        ))
  (def guitar-3 (mk-bar [[:e3 :d4 :d5] [:e3 :d4 :d5] [:e3 :d4 :d5] [:e3 :c#4 :c#5] [:e3 :c#4 :c#5] [:e3 :c#4 :c#5] [:e3 :d4 :d5] [:e3 :d4 :d5]]
                        (take 8 (repeat 3))
                        (take 8 (repeat 1/2))
                        (take 8 (range 0 4 1/2))
                        ))
  (def guitar-4 (mk-bar (take 8 (repeat [:e3 :b5 :b6]))
                        (take 8 (repeat 3))
                        (take 8 (repeat 1/2))
                        (take 8 (range 0 4 1/2))
                        ))

  (def the-bass-part [bass-e bass-a bass-e bass-a2])
  (def the-guitar-part [guitar-1 guitar-2 guitar-1 guitar-3 guitar-1 guitar-2 guitar-4 guitar-4])

  (def m (metronome 180))

  (play-bars (flatten (take 16 (repeat the-bass-part))) flute m)

  (play-bars the-guitar-part flute m :start-at 32)
  (play-bars the-guitar-part flute m :start-at 128)

  )
