;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.readme-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.reap.alpha.decoders.rfc7231 :refer [accept]]
   [juxt.reap.alpha.regex :as re]))

(deftest readme-test
  (is
   (=
    [#:juxt.reap.alpha.rfc7231{:media-range "text/html",
                               :type "text",
                               :subtype "html",
                               :parameters {}}
     #:juxt.reap.alpha.rfc7231{:media-range "application/xhtml+xml",
                               :type "application",
                               :subtype "xhtml+xml",
                               :parameters {}}
     #:juxt.reap.alpha.rfc7231{:media-range "application/xml",
                               :type "application",
                               :subtype "xml",
                               :parameters {},
                               :qvalue 0.9}
     #:juxt.reap.alpha.rfc7231{:media-range "image/webp",
                               :type "image",
                               :subtype "webp",
                               :parameters {}}
     #:juxt.reap.alpha.rfc7231{:media-range "image/apng",
                               :type "image",
                               :subtype "apng",
                               :parameters {}}
     #:juxt.reap.alpha.rfc7231{:media-range "*/*",
                               :type "*",
                               :subtype "*",
                               :parameters {},
                               :qvalue 0.8}
     #:juxt.reap.alpha.rfc7231{:media-range "application/signed-exchange",
                               :type "application",
                               :subtype "signed-exchange",
                               :parameters {"v" "b3"},
                               :qvalue 0.9}]

    (let [decoder (accept {})]
      (decoder (re/input "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9"))))))
