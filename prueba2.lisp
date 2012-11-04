;Variable Global para el directorio
(defvar *Directorio*)
(setf Directorio nil)

;;variable con los tags existentes en el id3 v3
(defparameter *tag* '(TIT2 TPE1 TALB TDRC APIC  APIC-1 APIC-2 APIC-3 COMM IPLS MCDI PCNT PRIV SYLT TBPM TCMP TCOM TCON TDAT TDLY TENC TEXT TFLT TIME TIT2 TIT3 TKEY TLAN TLEN TMED TOAL TOFN TOLY TOPE TORY TOWN TPE2 TPE3 TEP4 TPOS TPUB TRCK TRDA TRSN TRSO TSIZ TSRC TSSE TXXX TYER USER USLT WCOM WCOP WOAF WOAR WOAS WORS WPAY WPUB))

(defparameter *word* "tit2EnTuCuerpoTPE1SaratogaTALBVIITDRC2007TYER2007TRCK73.96")
(defparameter *ht* (make-hash-table))
(defparameter *archivo* nil)
(defparameter *byte* 0)
(defparameter *tag-size* 0)
(defparameter *tags* "")  

(defclass mp3-file ()
	((nombre :initarg :nombre :initform "")
	 (artista :initarg :artista :initform "")
	 (genero :initarg :genero :initform "")
	 (album :initarg :album :initform "")
	 ))
	
(defun print-nbr(a-mp3 nombre)
  (if (STRING-EQUAL nombre (slot-value a-mp3 'nombre))
    (progn 
 	 (format t (slot-value a-mp3 'nombre))
 	 (format t "~%")
 	 (format t (slot-value a-mp3 'artista))
 	 (format t "~%")
 	 (format t (slot-value a-mp3 'album))
 	 (format t "~%")
 	 (format t (slot-value a-mp3 'genero)) 
 	 (format t "~%")
	 (format t "~%")
))
)

(defun consulta-nbr (nombre) 
  (maphash #'(lambda (k v) (print-nbr v genero)) *ht*))


(defun print-gen(a-mp3 genero)
  (if (STRING-EQUAL genero (slot-value a-mp3 'genero))
    (progn 
 	 (format t (slot-value a-mp3 'nombre))
 	 (format t "~%")
 	 (format t (slot-value a-mp3 'artista))
 	 (format t "~%")
 	 (format t (slot-value a-mp3 'album))
 	 (format t "~%")
 	 (format t (slot-value a-mp3 'genero)) 
 	 (format t "~%")
	 (format t "~%")
))
)

(defun consulta-genero (genero) 
  (maphash #'(lambda (k v) (print-gen v genero)) *ht*))

(defun print-art(a-mp3 artista)
  (if (STRING-EQUAL artista (slot-value a-mp3 'artista))
    (progn 
 	 (format t (slot-value a-mp3 'nombre))
 	 (format t "~%")
 	 (format t (slot-value a-mp3 'artista))
 	 (format t "~%")
 	 (format t (slot-value a-mp3 'album))
 	 (format t "~%")
 	 (format t (slot-value a-mp3 'genero)) 
 	 (format t "~%")
	 (format t "~%")
))
)

(defun consulta-artista (artista) 
  (maphash #'(lambda (k v) (print-art v artista)) *ht*))
  
(defun print-alb(a-mp3 album)
  (if (STRING-EQUAL album (slot-value a-mp3 'album))
    (progn 
 	 (format t (slot-value a-mp3 'nombre))
 	 (format t "~%")
 	 (format t (slot-value a-mp3 'artista))
 	 (format t "~%")
 	 (format t (slot-value a-mp3 'album))
 	 (format t "~%")
 	 (format t (slot-value a-mp3 'genero)) 
 	 (format t "~%")
	 (format t "~%")
))
)

(defun consulta-album (album) 
  (maphash #'(lambda (k v) (print-alb v album)) *ht*))
  
(defun print-mp3 (a-mp3)
(progn 
 	 (format t (slot-value a-mp3 'nombre))
 	 (format t "~%")
 	 (format t (slot-value a-mp3 'artista))
 	 (format t "~%")
 	 (format t (slot-value a-mp3 'album))
 	 (format t "~%")
 	 (format t (slot-value a-mp3 'genero)) 
 	 (format t "~%")
	 (format t "~%")
))

(defun print-ht ()
 (maphash #'(lambda (k v) (print-mp3 v)) *ht*))

(defun lee-id3 (a-mp3)
  (setf *archivo* (open a-mp3 :element-type 'unsigned-byte))
  (setf *byte* 0)
  (setf *tag-size* 0)
  (setf *tags* "")
  (code-char (read-byte *archivo*));I
  (code-char (read-byte *archivo*));D
  (code-char (read-byte *archivo*));C 
  (read-byte *archivo*) ;major version
  (read-byte *archivo*) ;revision
  (read-byte *archivo*) ;flags
  (setf *tag-size* (+(*(read-byte *archivo*) 16777216) (*(read-byte *archivo*) 65536) (*(read-byte *archivo*) 256) (read-byte *archivo*))) ;4 bytes de tamaño de tags
  (dotimes (i 5000)
    (if (and (>= (setf *byte* (read-byte *archivo*)) 0) (<= *byte* 31))
	  nil
	  ;(format t "~a" (code-char *x*))))
	  (setf *tags* (concatenate 'string *tags* (princ-to-string(code-char *byte*))))))
          ;;Esto es lo q necesita para crear el mp3
	  (crear_mp3(split-by-$ (revisar *tags* *tag*)))
)

(defun carga-directorio(directorio)
 (loop for f in (directory (concatenate 'string directorio "/*.mp3"))
  collect (lee-id3 f)))

;;Revisa si el  string tiene tags y los envia remplazar
(defun revisar(string lista)
 (loop for i in lista do
  (cond 
   ((string-equal i  "TIT2")  (setf string (replaceString (string-downcase (string i)) (string-downcase string)  "$01$")));;titulo cancion
   ((string-equal i  "TPE2")  (setf string (replaceString (string-downcase (string i)) (string-downcase string)  "$02$")));;artista
   ((string-equal i  "TCON")  (setf string (replaceString (string-downcase (string i)) (string-downcase string)  "$03$")));;genero
   ((string-equal i  "TALB")  (setf string (replaceString (string-downcase (string i)) (string-downcase string)  "$04$")));;album
   (t (setf string (replaceString (string-downcase (string i)) (string-downcase string)  "$$$$")))
))string)

;;Remplaza los tags por comodines			
(defun replaceString (parte stringOriginal remplazo)
	(cond ((NULL (search parte stringOriginal)) stringOriginal)
		(T(setf (subseq stringOriginal (search parte stringOriginal) 
		(+ (search parte stringOriginal) (length parte))) remplazo) 
		(replaceString parte stringOriginal remplazo))))


;;Recibe un string y le hace un split sobre el caracter $
(defun split-by-$(string)    
    (loop for i = 0 then (1+ j)
          as j = (position #\$ string :start i)
          collect (subseq string i j)
          while j))


 ;;con esto lo prueba (split-by-$(revisar *word* *tag*))
(defun insert (a-mp3) (setf (gethash(slot-value a-mp3 'nombre) *ht*) a-mp3))
 
(defparameter *archivo2* (make-instance 'mp3-file :nombre "Judas" :artista "Lady Gaga" :genero "pop" :album "Born This Way"))

(defun crear_mp3 (lista &optional (Nombre "Sin nombre") (Artista "Sin Artista") (Genero "Sin genero") (Album "Sin album") )
 (cond
  ((eql lista '()) 
    (insert (make-instance 'mp3-file :nombre Nombre :artista Artista :genero Genero :album Album)))
  ((string-equal (first lista)  "01")
    (crear_mp3 (cdr lista) (first (cdr lista)) Artista Genero Album ))
  ((string-equal (first lista)  "02") 
    (crear_mp3 (rest lista) Nombre (first (rest lista))  Genero Album ))
  ((string-equal (first lista)  "03") 
    (crear_mp3 (rest lista) Nombre Artista  (first (rest lista)) Album ))
  ((string-equal (first lista)  "04") 
    (crear_mp3 (rest lista) Nombre Artista  Genero (first (rest lista))))
  (t 
    (crear_mp3 (rest lista) Nombre Artista Genero Album ))
 )
)

;Interfaz Grafica
(load "ltk")

;Funcion del boton cargar
(defun cargar-boton ()
	(ltk::clear-text *texto*)
	(carga-directorio (ltk::text *entry1*))
	(print-ht)
	(ltk::append-text *texto* "Los Mp3 han sido cargados exitosamente!! :)")
	(if (not (NULL directorio))
		(ltk::configure *entry1* :Text "No hay MP3 :(")
	)
)

;Funcion visualizar todo
(defun visualizar-todo (*ht*)
	(ltk::clear-text *texto*)
	(unless (= 0 (length *ht*))
	(ltk::append-text *texto* "|Canciones")
	(dotimes (j 
		(- (ELT *espacios* 0) 6))
		(ltk::append-text *texto* " ")
	)
))


(defun ventana-principal()
	(ltk::with-ltk ()
	(ltk::wm-title ltk::*tk*"Tarea programada #3")
	;Frame, texto y scroll para ventana de respuestas
	(defvar *frame2* (make-instance 'ltk::frame))
	(defvar *texto* (make-instance 'ltk::text :master *frame2* 
				:width 60 :height 30
				:wrap "none" ))
	(defvar *scrolly* (make-instance 'ltk::scrollbar :master *frame2*))
	(ltk::configure *texto* 
		:yscrollcommand 
		(concatenate 'string (ltk::widget-path *scrolly*) " set"))
	(ltk::configure *scrolly* 
		:command 
		(concatenate 'string (ltk::widget-path *texto*) " yview"))
	(defvar *scrollx* (make-instance 'ltk::scrollbar :master *frame2*
				 :orientation "horizontal"))
	(ltk::configure *texto* 
		:xscrollcommand 
		(concatenate 'string (ltk::widget-path *scrollx*) " set"))
	(ltk::configure *scrollx* 
		:command 
		(concatenate 'string (ltk::widget-path *texto*) " xview"))
	(ltk::grid *texto* 0 0 :sticky "snew")
	(ltk::grid *scrolly* 0 1 :sticky "ns")
	(ltk::grid *scrollx* 1 0 :sticky "ew")
	;fram para botones, entrys y labels
	(defvar *frame1* (make-instance 'ltk::frame))
	(ltk::grid *frame1* 0 0)
	(ltk::grid *frame2* 0 1)
	(defvar *boton1* (make-instance 'ltk::button 
		:Text "Cargar MP3"
		:master *frame1*
		:command (lambda () (cargar-boton))))
	(defvar *label1* (make-instance 'ltk::label 
		:Text "  Digite el directorio:
  EJ:/home/fer/Escritorio"
		:master *frame1*))
	(defvar *entry1* (make-instance 'ltk::entry 
		:master *frame1*
		:width 15))
	(ltk::grid *label1* 0 1 :Sticky "ns")
	(ltk::grid *boton1* 1 0 :Sticky "n")
	(ltk::grid *entry1* 1 1 :Sticky "n")
	(defvar *boton2* (make-instance 'ltk::button 
		:Text "Visualizar todas las etiquetas"
		:master *frame1*
		:command (lambda ()(visualizar-todo *ht*))))
	(ltk::append-text *texto* "Los Mp3 han sido cargados exitosaxcxzczxczxcmente!! :)")
	(ltk::grid *boton2* 2 0 :columnspan 2 :Sticky "w" )
	
	;Seccion BUsqueda
	(defvar *labelB* (make-instance 'ltk::label 
		:Text "Realizar busqueda por:"
		:master *frame1*))
	(ltk::grid *labelB* 3 0 :columnspan 2 :Sticky "w")

	(defvar *boton3* (make-instance 'ltk::button 
		:Text "Titulo"
		:master *frame1*
		:command (lambda () 
		(buscar-titulo (ltk::text *entry3*)))))
	(defvar *entry3* (make-instance 'ltk::entry 
		:master *frame1*
		:width 15))
	(ltk::grid *boton3* 4 0 :Sticky "n")
	(ltk::grid *entry3* 4 1 :Sticky "n")

	(defvar *boton4* (make-instance 'ltk::button 
		:Text "Artista"
		:master *frame1*
		:command (lambda () 
		(consulta-artista (ltk::text *entry4*)))))
	(defvar *entry4* (make-instance 'ltk::entry 
		:master *frame1*
		:width 15))
	(ltk::grid *boton4* 5 0 :Sticky "n")
	(ltk::grid *entry4* 5 1 :Sticky "n")

	(defvar *boton5* (make-instance 'ltk::button 
		:Text "Album"
		:master *frame1*
		:command (lambda () 
		(consulta-album (ltk::text *entry5*)))))
	(defvar *entry5* (make-instance 'ltk::entry 
		:master *frame1*
		:width 15))
	(ltk::grid *boton5* 6 0 :Sticky "n")
	(ltk::grid *entry5* 6 1 :Sticky "n")

	(defvar *boton6* (make-instance 'ltk::button 
		:Text "Genero"
		:master *frame1*
		:command (lambda () 
		(consulta-genero (ltk::text *entry6*)))))
	(defvar *entry6* (make-instance 'ltk::entry 
		:master *frame1*
		:width 15))
	(ltk::grid *boton6* 7 0 :Sticky "n")
	(ltk::grid *entry6* 7 1 :Sticky "n")
	)
)
(ventana-principal)



