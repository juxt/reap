;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.encoders.rfc7231
  (:require
   [clojure.string :as str]
   [juxt.reap.alpha.rfc7231 :as rfc]))

(declare imf-fixdate)
(declare obs-date)

;; HTTP-date = IMF-fixdate / obs-date
(defn http-date [opts]
  (let [imf-fixdate (imf-fixdate opts)
        ;;obs-date (obs-date opts)
        ]
    (fn [http-date]
      (imf-fixdate http-date))))

(defn imf-fixdate [_]
  (let [formatter (.withZone
                   java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME
                   (java.time.ZoneId/of "GMT"))]
    (fn [m]
      (when-let [^java.util.Date date (::rfc/date m)]
        (. formatter format (. date toInstant))))))


(defn vary [_]
  (fn vary-str [decoded]
    (cond
      (and (map? decoded) (contains? decoded ::rfc/wildcard))
      "*"
      (sequential? decoded)
      (->>
       (for [i decoded]
         (::rfc/field-name i))
       (str/join ", "))
      :else (throw (ex-info "Unrecognised vary data" {:arg decoded})))))

;; obs-date = rfc850-date / asctime-date
(defn obs-date [_]
  (fn [] (throw (ex-info "Unimplemented" {}))))
