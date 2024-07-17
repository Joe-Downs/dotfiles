(defun iso8601-date () "Get the current date (no time) in ISO 8601 format"
       (format "%04d-%02d-%02d"
               (decoded-time-year (decode-time))
               (decoded-time-month (decode-time))
               (decoded-time-day (decode-time))))
