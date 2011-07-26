(ns billsmith.util)

(defn get-settable-property-descriptors [clazz]
  "Returns a sequence of property descriptors for settable properties."
  (reduce (fn [coll m]
	    (let [name (.getName m)]
	      (if (and (java.lang.reflect.Modifier/isPublic (.getModifiers m))
		       (.startsWith name "set") 
		       (> (.length name) 3)
		       (Character/isUpperCase (.charAt name 3))
		       (= 1 (count (.getParameterTypes m))))
		(let [p-name (java.beans.Introspector/decapitalize (.substring name 3))]
		  (cons (doto (java.beans.PropertyDescriptor. p-name nil m) (.setWriteMethod m)) coll))
		coll)))
	  []
	  (.getMethods clazz)))

(defn set-bean [x props]
  "x is a bean and props is a map of settings to apply to that bean"
  (let [clazz (class x)
	pmap (reduce (fn [m pd]
		       (let [name (.getName pd)
			     method (.getWriteMethod pd)
			     types (. method (getParameterTypes))]
			 (if (and method (= 1 (alength types)))
			   (assoc m (keyword name) (fn [v] 
						     (try
						      (. method (invoke x (apply into-array (if (nil? v) [(first types) [v]] [[v]]))))
						      (catch IllegalArgumentException ex
							(throw (IllegalArgumentException. (str (.getMessage ex)  " (" name ")")))))))
			   m)))
		     {}
		     (get-settable-property-descriptors clazz))]
;;    (println "x is a " x " pmap is " pmap)
    (doseq [[k v] props]
	(let [f (k pmap)]
	  (if (nil? f)
	    (throw (Exception. (str "unknown property: " k)))
	    (f v))))
    x))
