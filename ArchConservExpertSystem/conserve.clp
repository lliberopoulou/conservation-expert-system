
(deffunction ask-question(?question $?allowed-values)
 (printout t ?question)
 (bind ?answer (read))
 (if (lexemep ?answer)
     then (bind ?answer (lowcase ?answer)))
(while (not (member ?answer ?allowed-values))do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer)
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffacts initial-phase
     (list intro))
     
(deftemplate
treatment (slot type))
 
(defrule startpr
 (list intro)
=>
(assert (ch1 storage) (ch2 clean) (ch3 encrust) (ch4 desal) (counter 0)))

(defrule first-quest
(counter 0)
=>
(printout t crlf)
(bind ?response (ask-question "Give to the object's physical integrity a value from 1 to 4 : " 1 2 3 4))
 (assert (physint ?response)))

(defrule sec-quest
 (physint ?response)
=>
(printout t crlf)
(bind ?response (ask-question "Give to the object's cohesivity a value from 1 to 4 : " 1 2 3 4))
 (assert (cohesv ?response)))

(defrule third-quest
 (cohesv ?response)
=>
(printout t crlf)
(bind ?response (ask-question "Give to the object's friability a value from 1 to 4 : " 1 2 3 4))
 (assert (friab ?response)))

(defrule present-options
(physint ?physint)
(cohesv ?cohesv)
(friab  ?friab)
?check1<-(ch1 ?ch1)
?check2<-(ch2 ?ch2)
?check3<-(ch3 ?ch3)
?check4<-(ch4 ?ch4)
?counter<-(counter ?num)
(not (ask-again))
=>
(assert (ask-again))
(printout t crlf)
 (bind ?response (ask-question "Select treatment you want to apply from the storage, desalination, encrustation cleaning and cleaning (storage desal encrust clean):" clean desal encrust storage))
(if (eq ?response ?ch1)
then (retract ?check1 ?counter)(assert (treatment (type ?response))(ch1 -)(counter(+ ?num 1))))
(if (eq ?response ?ch2)
then (retract ?check2 ?counter)(assert (treatment (type ?response))(ch2 -)(counter(+ ?num 1))))
(if (eq ?response ?ch3)
then (retract ?check3 ?counter)(assert (treatment (type ?response))(ch3 -)(counter(+ ?num 1))))
(if (eq ?response ?ch4)
then (retract ?check4 ?counter)(assert (treatment (type ?response))(ch4 -)(counter(+ ?num 1)))))

(defrule desal-test
?tempdes<-(treatment (type desal))
(physint 1)
(friab 4)
(not (contineu-desal))
(counter ?num)
(ch4 ?ch4)
=>
(printout t crlf)
(bind ?response (ask-question "If the object is completely mineralized and shows no signs of active corrosion it might no be necessary to desalinate. Answer 'yes' if you still wish to see the available desalination treatments for this object or 'no' if you wish to continue: " yes no))
(if (eq ?response no)
then (assert (dont-want-desal))(retract ?tempdes)
else (assert (continue-desal))))

(defrule organicmat
(or (treatment (type storage))(dont-want-storage))
(not (or (organic-material)(no-organic-material)))
=>
(printout t crlf)
(bind ?response (ask-question "Are there any traces of organic material on the object (yes/no): "yes no))
 (if (eq ?response yes)
 then (assert (organic-material))
 else (assert (no-organic-material))))

(defrule othermet
(or (organic-material)(no-organic-material))
(not (or (other-metal) (no-other-metal)))
=>
(printout t crlf)
(bind ?response (ask-question "Are there any other metals attached on the object (yes/no): "yes no))
 (if (eq ?response yes)
 then (assert (other-metal))
 else (assert (no-other-metal))))

(defrule print-file
?file<-(display ?tname)
=>
(printout t crlf)
(bind ?response (ask-question "Do you want more information about this treatment (yes/no): "yes no))
 (if (eq ?response yes)
then (printout t crlf)(bind ?filename (str-cat ?tname ".txt"))
   (if (not (open ?filename t-file "r"))
 then (printout t "no additional text available ")(printout t ?filename) (retract ?file)
   else (bind ?line (readline t-file))
(while (neq ?line EOF) do
(printout t ?line)
(printout t crlf)
(bind ?line (readline t-file)))
(printout t crlf)
(close t-file)
(retract ?file)))
else (retract ?file))

(defrule stab1
(treatment (type storage))
(cohesv 3|4)
(no-organic-material)
(no-other-metal)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable storage treatments for this object are desiccation, freezing, in soil and cold alk.sulphide solution (desicc freeze soil cold-alk): "  desicc freeze soil cold-alk))
(assert (stabtr ?response))
(assert (display ?response)))

(defrule stab2
(treatment (type storage))
(or(other-metal)(no-other-metal))
(organic-material)
(cohesv 3|4)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable storage treatments for this object are freezing and burial in soil (freeze soil): "   freeze soil))
(assert (stabtr ?response))
(assert (display ?response)))

(defrule stab3
(treatment (type storage))
(other-metal)
(no-organic-material)
(cohesv 3|4)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable storage treatments for this object are desiccation, freezing and burial in soil (desicc freeze soil): "  desicc freeze soil))
(assert (stabtr ?response))
(assert (display ?response)))

(defrule stab4
(treatment (type storage))
(cohesv 2)
(no-organic-material)
(no-other-metal)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable storage treatments for this object are dessication, cold alk.sulphide and burial in soil (desicc soil cold-alk): " desicc soil cold-alk))
(assert (stabtr ?response))
(assert (display ?response)))

(defrule stab5
(treatment (type storage))
(cohesv 1)
(organic-material)
(no-other-metal)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable storage treatments for this object is in soil (soil): " soil ))
(assert (stabtr ?response))
(assert (display ?response)))

(defrule stab6
(treatment (type storage))
(cohesv 1)
(other-metal)
(no-organic-material)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable storage treatments for this object is in soil (soil): " soil ))
(assert (stabtr ?response))
(assert (display ?response)))

(defrule stab7
(treatment (type storage))
(cohesv 2)
(other-metal)
(no-organic-material)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable storage treatments for this object are desiccation and burial in soil (desicc soil): " desicc soil ))
(assert (stabtr ?response))
(assert (display ?response)))

(defrule stab8
(treatment (type storage))
(cohesv 2)
(no-other-metal)
(organic-material)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable storage treatments for this object are and burial in soil (soil): " soil ))
(assert (stabtr ?response))
(assert (display ?response)))

(defrule stab9
(treatment (type storage))
(cohesv 1)
(no-organic-material)
(no-other-metal)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable storage treatments for this object burial in soil and immersion in cold alk.sulphide (soil cold-alk): " soil cold-alk ))
(assert (stabtr ?response))
(assert (display ?response)))

(defrule stab10
(treatment (type storage))
(cohesv 1|2)
(organic-material)
(other-metal)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable storage treatments for this object burial in soil (soil): " soil ))
(assert (stabtr ?response))
(assert (display ?response)))


(defrule check-storage
(not (or (treatment (type storage))(dont-want-storage)))
(not (dont-want-desal))
?counter<-(counter ?num)
?check1<-(ch1 ?ch1)
=>
(printout t crlf)
(retract ?counter)
(retract ?check1)
(assert (counter(+ ?num 1))(ch1 -))
(bind ?response (ask-question "If you have already applied or if you are going to apply storage before any other treatment you should specify which technique you are going to use. Enter 'yes' if you want to see available storage treatments for this object or 'no' if you wish to continue: " yes no))
(if (eq ?response yes)
 then  (assert (treatment (type storage)))
 else (assert (dont-want-storage))))

(defrule check-storage-without-desal
(or (treatment (type encrust))(treatment (type clean)))
(not (or (treatment (type storage)) (dont-want-storage)))
(dont-want-desal)
?counter<-(counter ?num)
?check1<-(ch1 ?ch1)
=>
(printout t crlf)
(retract ?counter)
(retract ?check1)
(assert (counter(+ ?num 1)) (ch1 -))
(bind ?response (ask-question "If you have already applied or if you are going to apply storage before any other treatment you should specify which technique you are going to use. Enter 'yes' if you want to see available storage treatments for this object or 'no' if you wish to continue: " yes no))
(if (eq ?response yes)
 then  (assert (treatment (type storage)))
 else (assert (dont-want-storage))))

(defrule check-encrust
(treatment (type desal)) 
(or (stabtr ?stabtr)(dont-want-storage))
(not (or (dont-want-encrust)(treatment (type encrust))))
?counter<-(counter ?num)
?check3<-(ch3 ?ch3)
=>
(printout t crlf)
(retract ?counter)
(retract ?check3)
(assert (counter(+ ?num 1))(ch3 -))
(bind ?response (ask-question "If the object is covered with encrustation it would be better to remove them before applying a desalination treatment. Enter 'yes' if you want to see available encrust. cleaning methods for this object or 'no' if you wish to continue: " yes no))
(if (eq ?response yes)
 then  (assert (treatment (type encrust)))
 else (assert (dont-want-encrust))))

(defrule desal1
(treatment (type desal))
(stabtr desicc)
(cohesv 3|4)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable treatments for this object are hydrogen plasma and lithium hydroxide (plasma lithium)" plasma lithium))
(assert (desalt ?response))
(assert (display ?response)))

(defrule desal2
(treatment (type desal))
(stabtr desicc)
(no-organic-material)
(no-other-metal)
(cohesv 1|2)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable desalination treatments for this object is lithium hydroxide (lithium): " lithium))
(assert (desalt ?response))
(assert (display ?response)))

(defrule desal3
(treatment (type desal))
(or (stabtr ?stabtr)(dont-want-storage))
(not (stabtr desicc))
(cohesv 3|4)
(no-organic-material)
(no-other-metal)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable desalination treatments for this object are hydrogen plasma, lithium hydroxide, alk.sulphite, boiling water and Soxhlet (plasma lithium alk-sulp soxhlet boil): " plasma lithium alk-sulp soxhlet boil))
(assert (desalt ?response))
(assert (display ?response)))

(defrule desal4
(treatment (type desal))
(or (stabtr ?stabtr)(dont-want-storage))
(not (stabtr desicc))
(cohesv 2) 
(no-organic-material)
(no-other-metal)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable desalination treatments for this object are Soxhlet, lithium hydroxide and dist. water (soxhlet plasma lithium water): " soxhlet lithium water))
(assert (desalt ?response))
(assert (display ?response)))

(defrule desal5
(treatment (type desal))
(or (stabtr ?stabtr)(dont-want-storage))
(cohesv 1) 
(no-organic-material)
(no-other-metal)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable desalination treatments for this object are Soxhlet, lithium hydroxide and dist. water (soxhlet lithium water): " soxhlet lithium water))
(assert (desalt ?response))
(assert (display ?response)))

(defrule desal6
(treatment (type desal))
(or (stabtr ?stabtr)(dont-want-storage))
(organic-material)
(no-other-metal)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable desalination treatments for this object are Soxhlet and dist. water (soxhlet water): " soxhlet water))
(assert (desalt ?response))
(assert (display ?response)))

(defrule desal7
(treatment (type desal))
(or (stabtr ?stabtr)(dont-want-storage))
(not (stabtr desicc))
(cohesv 1|2) 
(other-metal)
(no-organic-material)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable desalination treatments for this object which are the Soxhlet washing method and immersion in dist.water (soxhlet water): " soxhlet water))
(assert (desalt ?response))
(assert (display ?response)))

(defrule desal8
(treatment (type desal))
(stabtr desicc)
(cohesv 1|2) 
(other-metal)
(no-organic-material)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable desalination treatment(s) for this object which are the hydrogen plasma method (plasma) " plasma))
(assert (desalt ?response))
(assert (display ?response)))

(defrule desal9
(treatment (type desal))
(or (treatment (type storage))(dont-want-storage))
(cohesv 2) 
(other-metal)
(organic-material)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable desalination treatments for this object which are the Soxhlet washing method and immersion in dist.water (soxhlet water): " soxhlet water))
(assert (desalt ?response))
(assert (display ?response)))

(defrule desal10
(treatment (type desal))
(stabtr desicc)
(cohesv 3|4)
(other-metal)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable treatments for this object are hydrogen plasma and lithium hydroxide (plasma)" plasma))
(assert (desalt ?response))
(assert (display ?response)))

(defrule desal11
(treatment (type desal))
(or (treatment (type storage))(dont-want-storage))
(cohesv 3|4)
(other-metal)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable desalination treatments for this object are hydrogen plasma, boiling water and Soxhlet (plasma lithium alk-sulp soxhlet boil): " plasma soxhlet boil))
(assert (desalt ?response))
(assert (display ?response)))


(defrule washing
(not (dont-wash))
(or (or (desalt alk-sulp)(clean hexam))(or (clean oxalic)(clean ammonium)))
=>
(printout t crlf)
(bind ?response (ask-question "After applying the alk. sulphite or a chemical assisting a cleaning treatment you might need to remove any traces of the chemical from the object. Enter one of the available washing treatments Soxhlet, boiling and dist. water (soxhlet boil water) or type 'no' if you wish to continue (soxhlet boil water): " soxhlet boiling water no))
(if (neq no ?response)
then (assert (washing ?response)(display ?response)))
else (assert (dont-wash)))

(defrule clean1
(treatment (type clean))
(or (treatment (type storage))(dont-want-storage))
(cohesv 3)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable cleaning treatments for this object are air-abrasion sodium bicarbonate and scalpel (bicar scalpel): "  bicar scalpel))
(assert (clean ?response))
(assert (display ?response)))

(defrule clean2
(treatment (type clean))
(or (treatment (type storage))(dont-want-storage))
(cohesv 1)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable cleaning treatments for this object is air-abrasion with comprest air (air): "  air))
(assert (clean ?response))
(assert (display ?response)))

(defrule clean3
(treatment (type clean))
(or (treatment (type storage))(dont-want-storage))
(cohesv 4)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable cleaning treatments for this object are electric etcher-engraver, air-abrasion sodium bicarbonate and scalpel (etcher bicar scalpel): " etcher bicar scalpel))
(assert (clean ?response))
(assert (display ?response)))

(defrule clean4
(treatment (type clean))
(or (treatment (type storage))(dont-want-storage))
(cohesv 2)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable cleaning treatments for this object are electric etcher-engraver and air-abrasion with sodium bicarbonate (etcher bicar): " etcher bicar))
(assert (clean ?response))
(assert (display ?response)))

(defrule enclean1
(treatment (type encrust))
(or (treatment (type storage))(dont-want-storage))
(physint 3|4)
(cohesv 3|4)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable encrustation cleaning treatments for this object are miniature air-hammer,electric etcher-engraver, air-abrasion with aluminum oxide and scalpel (air-hammer alum etcher scalpel): " air-hammer alum etcher scalpel))
(assert (enclean ?response))
(assert (display ?response)))

(defrule enclean2
(treatment (type encrust))
(or (treatment (type storage))(dont-want-storage))
(not (stabtr desicc))
(physint 1|2)
(cohesv 2)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable treatments for this object are sodium hexametaphosphate oxalic acid, ammonium acetate, electric etcher-engraver, air abrasion with sodium bicarbonate and scalpel (hexam oxalic ammonium etcher bicar scalpel): "  hexam oxalic ammonium scribe bicar scalpel))
(assert (enclean ?response))
(assert (display ?response)))

(defrule enclean3
(treatment (type encrust))
(or (treatment (type storage))(dont-want-storage))
(cohesv 1)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable encrustation cleaning treatments for this object are sodium hexametaphosphate, oxalic acid, ammonium acetate and air abrasion with sodium bicarbonate (hexam oxalic ammonium bicar) : " hexam oxalic ammonium bicar ))
(assert (enclean ?response))
(assert (display ?response)))

(defrule enclean4
(treatment (type encrust))
(or (treatment (type storage))(dont-want-storage))
(physint 3|4)
(cohesv 2)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable encrustation cleaning treatments for this object are electric etcher-engraver, air abrasion aluminum oxide and scalpel (etcher alum scalpel): "  etcher alum scalpel))
(assert (enclean ?response))
(assert (display ?response)))

(defrule enclean5
(treatment (type encrust))
(or (treatment (type storage))(dont-want-storage))
(not (stabtr desicc))
(physint 1|2)
(cohesv 3|4)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable encrustation cleaning treatments for this object are sodium hexametaphosphate oxalic acid, ammonium acetate, electric etcher-engraver, air abrasion with aluminum oxide and scalpel (hexam oxalic ammonium etcher alum scalpel): " hexam oxalic ammonium etcher alum scalpel))
(assert (enclean ?response))
(assert (display ?response)))

(defrule enclean6
(treatment (type encrust))
(or (treatment (type storage))(dont-want-storage))
(stabtr desicc)
(physint 1|2)
(cohesv 2)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable treatments for this object are electric etcher-engraver, air abrasion with sodium bicarbonate and scalpel (etcher bicar scalpel): " etcher bicar scalpel))
(assert (enclean ?response))
(assert (display ?response)))

(defrule enclean7
(treatment (type encrust))
(or (treatment (type storage))(dont-want-storage))
(stabtr desicc)
(physint 1|2)
(cohesv 3|4)
=>
(printout t crlf)
(bind ?response (ask-question "Choose one of the acceptable encrustation cleaning treatments for this object are electric etcher-engraver, air abrasion with aluminum oxide and scalpel (etcher alum scalpel): " etcher alum scalpel))
(assert (enclean ?response))
(assert (display ?response)))


(defrule consolidate
(not (or (dont-want-consolidant)(consol ?consol)))
(cohesv 1|2) 
=>
(printout t crlf)
(bind ?response (ask-question "If the object is too brittle you could apply some synthetic consolidant to further stabilize it. Do you wish to see the materials available (yes/no)? " yes no))
(if (eq ?response yes)
then (printout t crlf)(bind ?response (ask-question "The available materials for the consolidation of iron are microcristalin wax and acrylic resin (micro acryl)? "  micro acryl))
(assert (consol ?response)(display ?response)))
else (assert(dont-want-consolidant)))

(defrule reconstruct
(not (or (dont-want-adhesive)(adhesv ?adhesv)))
(physint 1|2) 
=>
(printout t crlf)
(bind ?response (ask-question "If the object is broken into pieces you could apply some synthetic adhesive to reattach it. Do you wish to see the materials available (yes/no)? " yes no))
(if (eq ?response yes)
then (printout t crlf)(bind ?response (ask-question "The available materials for the adhesive of iron are cellulose nitrate and acrylic resin (nitro acryl)? "  nitro acryl))
(assert (adhesv ?response)(display ?response)))
else (assert(dont-want-adhesive)))

(defrule ask-again
(declare (salience -10))
(physint ?physint)
(cohesv ?cohesv)
(friab  ?friab)
(ch1 ?ch1)
(ch2 ?ch2)
(ch3 ?ch3)
(ch4 ?ch4)
(not (counter 4))
?question<-(ask-again)
=>
(printout t crlf)
(printout t "The treatments still available for this object are: " ?ch1 " " ?ch2 " " ?ch3 " " ?ch4)
(printout t crlf)
(printout t crlf)
(bind ?response (ask-question "Do you wish to apply any other treatment(s) on the object (yes/no)? " yes no))
(if (eq ?response yes)
then (retract ?question)(refresh present-options)
else (assert (new))))

(defrule new
(new)
=>
(printout t crlf)
(bind ?response (ask-question "Do you want to make a new query (yes/no)? " yes no))
(if (eq ?response yes)
then (reset)(run)
else (printout t crlf)(assert (haltpr))))

(defrule stop
(haltpr)
=>
(printout t "Goodbye" crlf)(halt))

(defrule end
(declare (salience -11))
=>
(printout t crlf)
(bind ?response (ask-question "Do you want to make a new query (yes/no)? " yes no))
(if (eq ?response yes)
then (reset)(run)
else (printout t crlf)(printout t "Goodbye" crlf)))




