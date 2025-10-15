;;; lib-weather.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar latitude "32.09703")
(defvar longitude "118.77969")

(defun fetch-weather-data (&rest _)
  "Fetch weather data from API and return weather string."
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url (format "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&daily=weather_code,temperature_2m_max,temperature_2m_min,sunrise,sunset,uv_index_max&timezone=Asia%%2FSingapore&forecast_days=1" latitude longitude)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (let* ((json-data (buffer-substring-no-properties (point) (point-max)))
             (json-obj (json-read-from-string json-data))
             (daily (cdr (assoc 'daily json-obj)))
             (weather-code (aref (cdr (assoc 'weather_code daily)) 0))
             (temp-max (aref (cdr (assoc 'temperature_2m_max daily)) 0))
             (temp-min (aref (cdr (assoc 'temperature_2m_min daily)) 0))
             (sunrise (substring (aref (cdr (assoc 'sunrise daily)) 0) 11))
             (sunset (substring (aref (cdr (assoc 'sunset daily)) 0) 11))
             (uv (uv-to-sunscreen-advice (aref (cdr (assoc 'uv_index_max daily)) 0)))
             (weather-description (weather-code-to-string weather-code))
             (weather-string (format "** : %s\n*** : %.1f°C-%.1f°C\n*** -: %s-%s\n*** UV: %s"
                                     weather-description temp-min temp-max sunrise sunset uv)))
        weather-string))))

(defun uv-to-sunscreen-advice (uv-index)
  "Return sunscreen advice based on the given UV index."
  (let ((uv-str (number-to-string uv-index)))
    (cond
     ((<= uv-index 2) (concat uv-str " 通常不需要特别防护，但可以考虑使用 SPF 15 的防晒霜。"))
     ((<= uv-index 5) (concat uv-str " 建议使用 SPF 15-30 的防晒霜，尤其是在户外活动时。"))
     ((<= uv-index 7) (concat uv-str " 建议使用 SPF 30-50 的防晒霜，并采取其他防护措施，如戴帽子和太阳镜。"))
     ((<= uv-index 10) (concat uv-str " 建议使用 SPF 50+的防晒霜，并尽量避免在阳光最强的时段外出，同时采取其他防护措施。"))
     ((>= uv-index 11) (concat uv-str " 强烈建议使用 SPF 50+的防晒霜，并采取一切可能的防护措施，如穿长袖衣物、戴帽子和太阳镜，尽量避免暴露在阳光下。"))
     (t "输入的 UV 指数无效。"))))

(defun weather-code-to-string (code)
  "Convert weather CODE to a human-readable string."
  (cond
   ((= code 0) "Clear sky")
   ((= code 1) "Mainly clear")
   ((= code 2) "Partly cloudy")
   ((= code 3) "Overcast")
   ((= code 45) "Fog")
   ((= code 48) "Depositing rime fog")
   ((= code 51) "Drizzle: Light")
   ((= code 53) "Drizzle: Moderate")
   ((= code 55) "Drizzle: Dense intensity")
   ((= code 56) "Freezing Drizzle: Light")
   ((= code 57) "Freezing Drizzle: Dense intensity")
   ((= code 61) "Rain: Slight")
   ((= code 63) "Rain: Moderate")
   ((= code 65) "Rain: Heavy intensity")
   ((= code 66) "Freezing Rain: Light")
   ((= code 67) "Freezing Rain: Heavy intensity")
   ((= code 71) "Snow fall: Slight")
   ((= code 73) "Snow fall: Moderate")
   ((= code 75) "Snow fall: Heavy intensity")
   ((= code 77) "Snow grains")
   ((= code 80) "Rain showers: Slight")
   ((= code 81) "Rain showers: Moderate")
   ((= code 82) "Rain showers: Violent")
   ((= code 85) "Snow showers: Slight")
   ((= code 86) "Snow showers: Heavy")
   ((= code 95) "Thunderstorm: Slight or moderate")
   ((= code 96) "Thunderstorm with slight hail")
   ((= code 99) "Thunderstorm with heavy hail")
   (t "Unknown weather condition")))

(provide 'lib-weather)
;;; lib-weather.el ends here
