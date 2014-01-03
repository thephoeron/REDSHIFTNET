;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*-
;;;; file: db-utils.lisp

;;;; Copyright (c) 2012 -- 2014 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;; Utilities for working with alists returned from postmodern
(defun get-alist-value (result key)
  "Return the value from the result for the given key on a list of alists."
  (cdr (assoc key result :test #'string-equal)))

;; Postmodern Utility Functions originally from: https://sites.google.com/site/sabraonthehill/postmodern-examples/exploring-a-database

(defun normalize-for-sql (string)
  "Substitutes underlines for hyphens."
  (substitute #\_ #\- string))

(defmacro make-list-query (relkind)
  "COPIED FROM POSTMODERN: Helper macro for the functions that list tables, 
sequences, and views."
  `(sql (:select 'relname :from 'pg-catalog.pg-class
                 :inner-join 'pg-catalog.pg-namespace :on (:= 'relnamespace 
                                                              'pg-namespace.oid)
                 :where (:and (:= 'relkind ,relkind)
                              (:not-in 'nspname (:set "pg_catalog" "pg_toast"))
                              (:pg-catalog.pg-table-is-visible 'pg-class.oid)))))

(defun database-version ()
  "Returns the version of the current postgresql database."
  (query (:select (:version)) :single))
  
(defun list-databases ()
  "This list drops the first two template databases."
  (let ((all-databases 
         (lol:flatten
          (query 
           (:order-by
            (:select 'datname
                     :from 'pg-database) 'datname)))))
    (nreverse
     (set-difference  all-databases (list "template0" "template1") 
                      :test 'equal))))

(defun list-databases-and-sizes ()
    "Returns a list of the databases and pretty-prints the sizes."
    (query 
     (:order-by 
        (:select 'datname 
                         (:pg-size-pretty 
                            (:pg-database-size 'pg-database.oid)) 
                         :from 'pg-database) 
        (:desc (:pg-database-size 'pg-database.oid)))))

(defun num-records-in-database ()
    "Returns a list of lists with schema, table name and approximate number of records in the currently connected database."
    (query (:order-by (:select 'schemaname 'relname 'n_live_tup 
                                                         :from 'pg_stat_user_tables)  
                                        (:desc 'n_live_tup))))

(defun current-database ()
  "Returns the string name of the current database"
  (query (:select (:current-database)) :single))

(defun database-exists-p (database-name)
  "Determine if a particular database exists"
      (if (symbolp database-name)
      (setf database-name
            (string-downcase (normalize-for-sql (string database-name))))
      (setf database-name (normalize-for-sql database-name)))
  (member database-name (list-databases) :test 'equal))

(defun current-database-size-pretty ()
  "Returns the current database size as a string in MB"
  (query (:select (:pg_size_pretty 
                   (:pg_database_size (:current_database)))) 
         :single))

(defun current-database-size ()
  "Returns the current database size in bytes"
  (query (:select (:pg_database_size (:current_database))) 
         :single))

(defun list-database-tables (&optional (schema-name "public"))
  "Returns a list of the tables in the schema in the currently connected database. The list is in string form."
  (lol:flatten (query
            (:order-by
             (:select 'table-name
                      :from 'information-schema.tables
                      :where (:= 'table-schema '$1))
             'table-name)
            schema-name)))

(defun list-all-table-sizes ()
  "Returns a list of lists (table-name, size in 8k pages) of all tables, indexes 
etc in the database in descending order. Includes system tables, so there 
are a lot more than you would expect"
  (query (:order-by (:select 'relname 'relpages 
                             :from 'pg_class) 
                    (:desc 'relpages))))

(defun table-size (table-name)
  "Return the size of a postgresql table in k or m. Table-name can be either a
string or quoted."
    (when (symbolp table-name)    
        (setf table-name  (string-downcase (write-to-string table-name))))
  (query (:select (:pg_size_pretty (:pg_total_relation_size'$1)))
                 :single
                 table-name))

(defun more-table-info (table-name)
  "Returns more table info than table-description. Table can be either a
string or quoted."
    (if (symbolp table-name)
    (setf table-name  (string-downcase
                       (normalize-for-sql (string table-name))))
    (setf table-name (normalize-for-sql table-name)))
  (query (:order-by (:select (:as 'a.attnum 'ordinal-position) 
                             (:as 'a.attname 'column-name)
                             (:as 'tn.typname 'data-type)
                             (:as 'a.attlen  'character-maximum-length)
                             (:as 'a.atttypmod 'modifier)
                             (:as 'a.attnotnull 'notnull)
                             (:as 'a.atthasdef 'hasdefault)
                             :from (:as 'pg_class 'c)
                             (:as 'pg_attribute 'a)
                             (:as 'pg_type 'tn)
                             :where (:and
                                     (:= 'c.relname '$1)
                                     (:> 'a.attnum 0)
                                     (:= 'a.attrelid 'c.oid)
                                     (:= 'a.atttypid 'tn.oid)))
                    'a.attnum)
         table-name))

(defun more-table-info-info-schema (table-name)
  "Returns more table info than table-description, but using the
information_schema tables. Notice several differences between this
and more-table-info. First, this returns  'integer' or 'nextval(sequence-name0'
rather than int4, :NULL rather than NIL, and the max-length results are different.
Table can be either a string or quoted."
    (if (symbolp table-name)
      (setf table-name
            (string-downcase (normalize-for-sql (string table-name))))
      (setf table-name (normalize-for-sql table-name)))
  (query (:order-by (:select 'ordinal-position 'column-name 'data-type
                             'character-maximum-length 'is-nullable
                             'column-default 'numeric-precision
                             :from 'information-schema.columns
                             :where (:= 'table-name '$1))
                    'ordinal-position)
         table-name))

(defun list-database-users () 
  "List Database Users."
  (query (:order-by (:select 'usename
                             :from 'pg_user)
                    'usename)))

(defun list-schemas ()
    "List schemas in the current database"
    (flatten (query (:select 'nspname
                           :from 'pg_namespace 
                           :where (:!~* 'nspname "^pg_.*")))))

(defun list-schema-table-type-owner (relkind-type)
    "Returns a list of lists showing the schema, the name, the type and the owner
where relkind-type is a list of strings where the strings are: c,r,v,i,S,c,t or f"
    (query (:order-by (:select (:as 'n.nspname 'Schema)
                                    (:as 'c.relname 'Name)
                                    (:as (:case ((:= 'c.relkind "r") "Table") 
                                                 ((:= 'c.relkind "v") "view") 
                                                 ((:= 'c.relkind "i") "index")
                                                 ((:= 'c.relkind "S") "sequence")
                                                 ((:= 'c.relkind "c") "composite")
                                                 ((:= 'c.relkind "t") "TOAST")
                                                 ((:= 'c.relkind "f") "foreign"))
                                             'Type)
                                    (:as 'u.usename 'Owner)
                                    (:as (:/ (:pg_total_relation_size 'c.oid) 1000) 'Size) 
                                    (:as 'c.reltuples 'Records)
                                    (:as 'c.relhasindex 'Indexed)
                                    (:as 'c.relchecks 'Constraints)
                                    (:as 'c.relhastriggers 'Triggers)
                                    (:as (:pg_size_pretty (:pg_total_relation_size 'c.oid))
                       'Size)
                                    :from (:as 'pg-catalog.pg-class 'c)
                                    :left-join (:as 'pg-catalog.pg-user    'u)
                                    :on (:= 'u.usesysid  'c.relowner)
                                    :left-join (:as 'pg-catalog.pg-namespace    'n)
                                    :on (:= 'n.oid  'c.relnamespace)
                                    :where (:and (:in 'c.relkind (:set relkind-type ""))
                                                             (:not-in 'n.nspname
                                        (:set "pg_catalog" "pg-toast"))
                                                             (:pg-catalog.pg-table-is-visible 'c.oid)))
                    1 2)))

(defun list-columns (table-name)
  "Returns a list of strings of just the column names in a table.
Pulls info from the postmodern table-description function
rather than directly."
  (when (table-exists-p table-name)
    (let ((field-list ()))
      (dolist (x (postmodern:table-description table-name))
        (push (first x) field-list))
      (nreverse field-list))))

(defun list-columns-with-types (table-name)
  "Return a list of (name type) lists for the fields of a table. Goes
directly to the pg-catalog tables."
  (if (symbolp table-name)
      (setf table-name
            (normalize-for-sql
             (string-downcase (string table-name))))
      (setf table-name (normalize-for-sql table-name)))
  (when (table-exists-p table-name)
    (query 
     (:select (:as 'a.attname 'column)
              (:as (:pg-catalog.format_type 'a.atttypid  'a.atttypmod)
                   'datatype)
              :from (:as 'pg-catalog.pg-attribute 'a)
              :where (:and
                      (:> 'a.attnum 0)
                      (:not 'a.attisdropped)
                      (:= 'a.attrelid
                          (:select 
                           'c.oid
                           :from (:as 'pg-catalog.pg-class 'c)
                           :left-join (:as 'pg-catalog.pg-namespace 'n)
                           :on (:= 'n.oid 'c.relnamespace)
                           :where (:and
                                   (:= 'c.relname '$1)
                                   (:pg-catalog.pg-table-is-visible 'c.oid))))))
     table-name)))

(defun column-exists-p (table-name column-name) 
  "Determine if a particular column exists. Table name and column-name
can be either strings or symbols."
    (if (symbolp table-name)    
      (setf table-name  (string-downcase
                         (normalize-for-sql (string table-name))))
      (setf table-name (normalize-for-sql table-name)))
    (if (symbolp column-name)    
      (setf column-name  (string-downcase
                          (normalize-for-sql (string column-name))))
      (setf column-name (normalize-for-sql column-name)))
    (query (:select 'attname :from 'pg_attribute 
                      :where (:= 'attrelid 
                                 (:select 'oid :from 'pg-class 
                                          :where (:and (:= 'relname '$1) 
                                                       (:= 'attname '$2)))))
             table-name column-name))

(defun list-tables-with-column (column-name)
    "Returns a list of table names where the tables have the column parameter.
Column-name can be either a symbol or a string. "
  (when (symbolp column-name)
    (setf column-name (normalize-for-sql
                       (string-downcase (string column-name)))))
    (flatten (query (:select 'table-name
                                                     :from 'information-schema.columns
                                                     :where (:= 'column-name '$1))
                  column-name)))

(defun list-database-functions ()
  "Returns a list of the functions in the database from the information_schema"
  (query (:select 'routine-name :from 'information-schema.routines
                  :where
                  (:and
                   (:not-in 'specific-schema 
                            (:set "pg_catalog" "information-schema"))
                   (:!= 'type-udt-name "trigger")))))

(defun describe-views ()
    "Describe the current views in schema public"
    (query
     (:order-by
        (:select 'c.oid 'c.xmin 'c.relname
                         (:as (:pg_get_userbyid 'c.relowner) 'viewowner)
                         'c.relacl 'description
                         (:as (:pg_get-viewdef 'c.oid 't) 'code)
                         :from (:as 'pg_class 'c)                         
                         :left-join (:as 'pg_description 'des)
                         :on (:and (:= 'des.objoid 'c.oid)
                                             (:= 0 'des.objsubid))
                         :left-join (:as 'pg_catalog.pg_namespace 'n)
                         :on (:= 'n.oid 'c.relnamespace)
                         :where
             (:and
              (:or
               (:and 'c.relhasrules
                     (:exists
                      (:select 'r.rulename
                               :from (:as 'pg_rewrite 'r)
                               :where (:and (:= 'r.ev_class 'c.oid)
                                            (:= (:bpchar 'r.ev_type)
                                                (:type "I" bpchar))))))
               (:= 'c.relkind (:type "v" char)))
              (:= 'n.nspname "public")))
        'relname)))

(defun list-indices (&optional strings-p)
  "Return a list of the indexs in a database. Turn them into keywords
if strings-p is not true."
  (let ((result (query (make-list-query "i") :column)))
    (if strings-p result (mapcar 'from-sql-name result))))

(defun list-table-indices (table-name &optional strings-p)
  "List the index names in a table. Does not include primary or unique keys"
  (when (table-exists-p table-name)
    (let ((result (flatten
                   (query 
                    (:select 
                     'relname 
                     :from 'pg_class
                     :where (:in 'oid
                                 (:select 
                                  'indexrelid
                                  :from 'pg_index 'pg_class
                                  :where (:and
                                          (:= 'pg_class.relname table-name)
                                          (:= 'pg_class.oid 'pg_index.indrelid)
                                          (:!= 'indisunique "t")
                                          (:!= 'indisprimary "t")))))))))
      (if strings-p result (mapcar 'from-sql-name result)))))


(defun list-indexed-column-and-attributes (table-name) 
  "List the indexed columns and their attributes in a table. Includes primary key."
  (query
   (:select 'pg_attribute.attname 
            (:format_type 'pg_attribute.atttypid 'pg_attribute.atttypmod) 
            :from 'pg_index 'pg_class 'pg_attribute 
            :where (:and (:= 'pg_class.oid (:type '$1 :regclass)) 
                         (:= 'indrelid 'pg_class.oid)
                         (:= 'pg_attribute.attrelid 'pg_class.oid)
                         (:= 'pg_attribute.attnum
                             (:any* 'pg_index.indkey)))) 
   table-name))

(defun list-index-definitions (table-name)
  "Returns a list of the definitions used to create the current indexes for
the table"
    (if (symbolp table-name)    
      (setf table-name
            (string-downcase (normalize-for-sql (string table-name))))
      (setf table-name
            (string-downcase (normalize-for-sql table-name))))
  (when (table-exists-p table-name)
    (query (:select (:pg_get_indexdef 'indexrelid) 
                    :from 'pg_index 
                    :where (:= 'indrelid (:type '$1 :regclass)))
                     table-name)))

(defun list-foreign-keys (table-name)
  "List the foreign keys in a table"
  (query (:select 'tc.constraint_name
                  'tc.table_name 'kcu.column_name
                  (:as 'ccu.table_name 'foreign_table_name)
                  (:as 'ccu.column_name 'foreign_column_name)
                  :from (:as 'information_schema.table_constraints 'tc)
                  :inner-join
                  (:as 'information_schema.key_column_usage 'kcu)
                  :on (:= 'tc.constraint_name 'kcu.constraint_name)
                  :inner-join
                  (:as 'information_schema.constraint_column_usage 'ccu)
                  :on (:= 'ccu.constraint_name 'tc.constraint_name)
                  :where (:and (:= 'constraint_type "FOREIGN KEY")
                               (:= 'tc.table_name '$1)))
         table-name))

(defun list-unique-or-primary-constraints (table-name)
  "List constraints on a table"
  (when (table-exists-p table-name)
    (query (:select 
            'relname
            :from 'pg-class
            :where 
            (:in 'oid (:select 'indexrelid
                               :from 'pg-index 'pg-class
                               :where (:and
                                       (:= 'pg-class.relname '$1)
                                       (:= 'pg-class.oid 'pg-index.indrelid)
                                       (:or (:= 'indisunique "t")
                                            (:= 'indisprimary "t"))))))
           table-name)))

(defun list-all-constraints (table-name)
  "Users information_schema to list all the constraints in a table. Table-name
can be either a string or quoted."
    (when (symbolp table-name)
        (setf table-name  (string-downcase (write-to-string table-name))))
    (when (table-exists-p table-name)
      (query (:select 'constraint-name 'constraint-type
                      :from 'information-schema.table-constraints
                      :where (:= 'table-name '$1))
                      table-name)))

(defun describe-constraint (table-name constraint-name)
  "Return a description list of a particular constraint given
the table-name and the  constraint name using the information_schema 
table."
    (if (symbolp table-name)    
      (setf table-name
            (string-downcase (normalize-for-sql (string table-name))))
      (setf table-name
            (string-downcase (normalize-for-sql table-name))))    
  (when (table-exists-p table-name)
    (first 
     (query 
      (:select 'tc.constraint-name
               'tc.constraint-type
               'tc.table-name
               'kcu.column-name
               'tc.is-deferrable
               'tc.initially-deferred
               (:as 'rc.match-option 'match-type)
               (:as 'rc.update-rule 'on-update)
               (:as 'rc.delete-rule 'on-delete)
               (:as 'ccu.table-name 'references-table)
               (:as 'ccu.column-name 'references-field)
               :from (:as 'information-schema.table-constraints 'tc)
               :left-join (:as 'information-schema.key-column-usage 'kcu)
               :on (:and (:= 'tc.constraint-catalog 'kcu.constraint-catalog)
                         (:= 'tc.constraint-schema 'kcu.constraint-schema)
                         (:= 'tc.constraint-name 'kcu.constraint-name))
               :left-join (:as 'information-schema.referential-constraints
                               'rc)
               :on (:and (:= 'tc.constraint-catalog 'rc.constraint-catalog)
                         (:= 'tc.constraint-schema 'rc.constraint-schema)
                         (:= 'tc.constraint-name 'rc.constraint-name))
               :left-join (:as 'information-schema.constraint-column-usage
                               'ccu)
               :on (:and (:= 'rc.unique-constraint-catalog
                             'ccu.constraint-catalog)
                         (:= 'rc.unique-constraint-schema
                             'ccu.constraint-schema)
                         (:= 'rc.unique-constraint-name 'ccu.constraint-name))
               :where (:and (:= 'tc.table-name (normalize-for-sql '$1))
                            (:= 'tc.constraint-name 
                                (normalize-for-sql constraint-name))))
            table-name))))

(defun describe-foreign-key-constraints ()
  "Generates a list of lists of information on the foreign key constraints"
  (query (:order-by (:select 'conname 
                             (:as 'conrelid 'table) 
                             (:as 'pgc.relname 'tabname) 
                             (:as 'a.attname 'columns)
                             (:as 'confrelid 'foreign-table) 
                             (:as 'pgf.relname 'ftabname)
                             (:as 'af.attname 'fcolumn)
                             :from 
                             (:as 'pg_attribute 'af)
                             (:as 'pg_attribute 'a)
                             (:as 'pg_class 'pgc) 
                             (:as 'pg_class 'pgf)
                             (:as 
                              (:select 'conname 'conrelid 'confrelid
                                       (:as (:[] 'conkey 'i) 'conkey)
                                       (:as (:[] 'confkey 'i) 'confkey)
                                       :from (:as 
                                              (:select 
                                               'conname
                                               'conrelid 'confrelid
                                               'conkey 'confkey
                                               (:as 
                                                (:generate-series 
                                                 '1 
                                                 (:array-upper 'conkey 1))
                                                'i)
                                               :from 'pg_constraint 
                                               :where (:= 'contype "f" )) 
                                              'ss) 
                                       ) 'ss2)
                             :where (:and (:= 'af.attnum 'confkey)
                                          (:= 'af.attrelid 'confrelid)
                                          (:= 'a.attnum 'conkey)
                                          (:= 'a.attrelid 'conrelid)
                                          (:= 'pgf.relfilenode 'confrelid)
                                          (:= 'pgc.relfilenode 'conrelid))) 
                    'ftabname 'fcolumn 'tabname 'columns)))

(defun list-triggers (&optional table-name)
  "List distinct trigger names from the information_schema table. Table-name can be
either quoted or string."
  (if table-name
    (progn
      (when (symbolp table-name)    
        (setf table-name  (string-downcase (write-to-string table-name))))
      (when (table-exists-p table-name)
        (lol:flatten (query 
          (:select (:as 'trg.tgname 'trigger-name) 
            :from (:as 'pg-trigger 'trg) (:as 'pg-class 'tbl)
            :where (:and (:= 'trg.tgrelid 'tbl.oid)
                      (:= 'tbl.relname '$1)))
          table-name))))
    (lol:flatten (query 
      (:select 'trigger-name :distinct
        :from 'information-schema.triggers
        :where 
        (:not-in 'trigger-schema 
          (:set "pg-catalog" "information-schema")))))))

(defun list-detailed-triggers ()
  "list detailed information on the triggers from the information_schema table"
  (query 
   (:select '* 
            :from 'information-schema.triggers
            :where 
            (:not-in 'trigger-schema 
                     (:set "pg_catalog" "information_schema")))))

(defun list-tablespaces ()
    "Lists the tablespaces in the currently connected database"
    (query (:order-by (:select (:as 'spcname 'name) :from 'pg_tablespace)
                                        'spcname)))

(defun list-types-in-database ()
    "list the available types in the database."
    (query (:select 'oid (:as (:format-type :oid :NULL) 'typename)
                                    :from 'pg-type :where (:= 'typtype "b"))))

;; EOF
