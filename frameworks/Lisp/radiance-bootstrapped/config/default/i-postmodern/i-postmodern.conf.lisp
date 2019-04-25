; meta (:version 1.0 :package "COMMON-LISP-USER")
[hash-table equal (:default "tfb")
 (:connections
  [hash-table equal
   ("tfb"
    [hash-table equal (:host "tfb-database") (:port 5432)
     (:database "hello_world") (:user "benchmarkdbuser")
     (:pass "benchmarkdbpass")])])]
