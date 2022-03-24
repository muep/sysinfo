;; MIT License
;;
;; Copyright (c) 2022 Joonas Sarajärvi
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(ns sysinfo
  (:require [clojure.string :as str])
  (:import (java.lang.management GarbageCollectorMXBean
                                 ManagementFactory
                                 MemoryUsage)))

(defn map-values [m f] (into {} (map (fn [[k, v]] [k (f v)]) m)))

(defn bytes->kib [b]
  (-> b (/ 1024) int))

(defn str->cpu-time [s]
  (-> s Integer/parseInt (/ 100) double))

(defn str->ram-kib [s]
  (-> s Integer/parseInt (* 4)))

;; Stock slurp gets confused by something in the behavior of procfs,
;; but opening a FileInputStream and slurping that seems to work ok.
(defn slurp-proc [path]
  (with-open [f (java.io.FileInputStream. path)]
    (slurp f)))

(defn pick-keys [m key-map]
  (into {} (map (fn [[kout kin]] [kout (get m kin)]) key-map)))


(defn proctbl->map [txt]
  (as-> txt v
    (str/split v #"\n")
    (map (fn [row] (str/split row #":\s*")) v)
    (into {} v)))

(defn strip-suffix [txt suffix]
  (when (.endsWith txt suffix)
    (.substring txt 0 (- (.length txt) (.length suffix)))))

(defn meminfo-str->int [num-and-unit]
  (if-let [kib-text (strip-suffix num-and-unit " kB")]
    (-> kib-text Long/parseLong (* 1024M))))

(def pool-names
  {"CodeHeap 'non-profiled nmethods'" :codeheap-non-profiled-nmethods
   "CodeHeap 'profiled nmethods'"     :codeheap-profiled-nmethods
   "CodeHeap 'non-nmethods'"          :codeheap-non-nmethods
   "Metaspace"                        :metaspace
   "Compressed Class Space"           :compressed-class-space
   "G1 Eden Space"                    :g1-eden-space
   "G1 Old Gen"                       :g1-old-gen
   "G1 Survivor Space"                :g1-survivor-space
   "G1 Old Generation"                :g1-old-generation
   "G1 Young Generation"              :g1-young-generation
   "Copy"                             :copy
   "MarkSweepCompact"                 :mark-sweep-compact
   "Tenured Gen"                      :tenured-gen
   "Eden Space"                       :eden-space
   "Survivor Space"                   :survivor-space})

(defn pool-name [p]
  (get pool-names p p))

(defn memory-usage->map [^MemoryUsage x]
  {:init (.getInit x)
   :used (.getUsed x)
   :committed (.getCommitted x)
   :max (.getMax x)})

(defn heap-stat []
  (let [mxb (ManagementFactory/getMemoryMXBean)
        heap-usage (-> mxb .getHeapMemoryUsage memory-usage->map)
        nonheap-usage (-> mxb .getNonHeapMemoryUsage memory-usage->map)
        pool (into {}
                   (map (fn [b] [(-> b .getName  pool-name)
                                 (-> b .getUsage memory-usage->map)])
                        (ManagementFactory/getMemoryPoolMXBeans)))
        gc (into {} (map (fn [^GarbageCollectorMXBean b]
                           [(-> b .getName pool-name)
                            {:count (.getCollectionCount b)
                             :time (.getCollectionTime b)}])
                         (ManagementFactory/getGarbageCollectorMXBeans)))]
    {:gc gc
     :heap-usage heap-usage
     :nonheap-usage nonheap-usage
     :pool pool}))

(def meminfo-fields {:mem-total "MemTotal"
                     :mem-free "MemFree"
                     :mem-available "MemAvailable"
                     :buffers "Buffers"
                     :cached "Cached"
                     :swap-total "SwapTotal"
                     :swap-free "SwapFree"})

(defn meminfo []
  (-> (slurp-proc "/proc/meminfo")
      proctbl->map
      (pick-keys meminfo-fields)
      (map-values meminfo-str->int)))

(defn proc-stat [proc]
  (let [pieces (-> (slurp-proc (str "/proc/" proc "/stat"))
                   (str/split #"\s+"))
        user (-> pieces (get 13) str->cpu-time)
        sys (-> pieces (get 14) str->cpu-time)]
    {:pid (-> pieces (get 0) Integer/parseInt)
     :rss (-> pieces (get 23) str->ram-kib)
     :user user
     :sys sys
     :time (+ user sys)}))

(defn runtime-stat []
  (let [rt (Runtime/getRuntime)]
    {:cpu-count (.availableProcessors rt)
     :free-memory (.freeMemory rt)
     :max-memory (.maxMemory rt)
     :total-memory (.totalMemory rt)
     :version (.toString (Runtime/version))}))

(defn sys-stat []
  {:heap (heap-stat)
   :meminfo (meminfo)
   :process (proc-stat "self")
   :runtime (runtime-stat)})

(defn sys-summary []
  (let [st (sys-stat)
        heap-size (-> st :heap :heap-usage :max bytes->kib)
        heap-used (-> st :heap :heap-usage :used bytes->kib)]
    {:process-id (-> st :process :pid)
     :jvm-nonheap-kib (-> st :heap :nonheap-usage :used bytes->kib)
     :jvm-heap {:size-kib heap-size
                :used-kib heap-used
                :utilization (-> heap-used (* 100) (/ heap-size) Math/ceil)}
     :cpu-seconds (-> st :process :time)
     :linux-mem-available-mib (-> st :meminfo :mem-available (/ 1048576.0))
     :process-rss-mib (-> st :process :rss (/ 1024.0))}))
