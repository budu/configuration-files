(ns custom-repl)

(defn ns-unmap-all
  "Removes all mappings from the specified namespaces, use the current
  namespace if none given."
  [& nss]
  (doseq [ns (if (empty? nss) [*ns*] nss)]
    (doseq [sym (keys (ns-interns ns))]
      (ns-unmap ns sym)))
  nil)

(in-ns 'user)

(refer 'custom-repl)
