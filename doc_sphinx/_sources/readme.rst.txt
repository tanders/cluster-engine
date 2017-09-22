========
Overview
========

    :Author: torsten

.. contents::



Introduction
------------

Cluster Engine is a constraint solver for solving polyphonic constraint satisfaction problems where both the pitch and the rhythmic structure can be restricted by an arbitrary number of constraints (rules), and the solver then searches for a solution that is consistent with all constraints. This library supports user-defined rules, and highly flexible ways to control which aspects of the resulting score are controlled by certain rules. For example, you can independently control with compositional rules the melody and harmony of the music you generate. 

Cluster Engine is developed by Örjan Sandred . It is the successor of PWMC (Sandred, 2010). Cluster Engine was originally developed as a library for the free composition environment `PWGL <http://www2.siba.fi/pwgl>`_. 

The present version still runs in PWGL, but also on plain Common Lisp in order to make it useable within `Opusmodus <http://opusmodus.com>`_ and -- via a `SBCL <http://www.sbcl.org>`_ interface -- in the music and media programming environment `Max <https://cycling74.com/products/max>`_ (Vincenot, 2017).

Cluster Engine has been successfully tested on PWGL (based on Lispworks), SBCL and Opusmodus (`Clozure CL <http://ccl.clozure.com>`_).

References
~~~~~~~~~~

Sandred, Ö. (2010) PWMC, a Constraint-Solving System for Generating Music Scores. *Computer Music Journal*. 34(2), 8–24.

Vincenot, J. (2017) LISP in Max: Exploratory Computer-Aided Composition in Real-Time. *ICMC 2017*.  

Installation
------------

PWGL
~~~~

These instructions explain how to install Cluster Engine for PWGL. First, download the library with the download link at `http://github.com/tanders/cluster-engine <http://github.com/tanders/cluster-engine>`_ and unzip the folder. Then, make sure the folder is named ``cluster-engine`` (remove the ``master`` part of the folder name). Finally, move this folder inside the PWGL user library folder (``~/PWGL-User/User-library/``). 

Common Lisp
~~~~~~~~~~~

This section explain how to install Cluster Engine as a plain Common Lisp library (e.g., to be used by Opusmodus). We are using git for the installation. Even though this is a bit more involved at first, it allows for convenient updates later, and you can even contribute to the development. 

Install `git <https://git-scm.com>`_ (if you have not done already). Also, you should register at `GitHub <https://github.com>`_.

Download the present software with git into a directory where `ASDF <https://common-lisp.net/project/asdf/>`_ can find the software, e.g., `~/common-lisp/ <https://common-lisp.net/project/asdf/asdf/Quick-start-summary.html#Quick-start-summary>`_. For example, on UNIX incl. OS X you can enter the following commands at the command line (after you created the directory ``~/common-lisp/``). Note that ``$`` is the terminal prompt here, you do not need to enter that :)

.. code:: bash

    $ cd ~/common-lisp
    $ git clone https://github.com/tanders/cluster-engine.git

You will be asked for your GitHub username and password.

Updating your software
~~~~~~~~~~~~~~~~~~~~~~

If there are `changes <https://github.com/tanders/cluster-engine/commits/master>`_, you can update your software later at the terminal in the following way.

.. code:: bash

    $ cd ~/common-lisp/cluster-engine
    $ git pull

Usage
-----

PWGL
~~~~

If you use Cluster Engine in PWGL, you load the library in PWGL with File > Load Library...

The library comes with a tutorial, which you find under Help > PWGL Tutorial... In the tutorial browser that opens then move to Library Tutorials > cluster-engine. The tutorial consists of a collection of live patches with comments and documentation slides. 

Common Lisp
~~~~~~~~~~~

Cluster Engine is an `ASDF <https://common-lisp.net/project/asdf/>`_ system (ASDF is the de facto standard for building Common Lisp software), and you can load it into your Lisp compiler as follows.

.. code:: common-lisp

    (require :cluster-engine)

The library should now be loaded, and you can solve musical constraint satisfaction problems. The first example below is an "empty" problem without any constraints yet. The function ``clusterengine`` is the constraint solver. Its first three arguments specify the number of variables (quasi notes) per voice (``10`` in this example), whether or not to randomise the solution (``T``) and whether to use additional debugging features (``nil``).  After a list of constraints (empty in this first example) the function expects specifications for various domains. These specifications support a mini language for flexibility, the specifications below are very simple. Only a single time signature is allowed (all bars are in 3/4 time); the single voice can consist of quarter notes and eighth notes; and possible pitches are middle C and C# (MIDI note numbers 60 and 61).  

.. code:: common-lisp

    (ce::clusterengine 
     10 t nil           ; settings
     nil                ; no constraints
     '((3 4))           ; metric domain
     ;; domain of rhythmic values and pitches for one voice
     '(((1/4) (1/8))   
       ((60) (61))))

``clusterengine`` outputs a declaration of the sequences of time signatures, rhythmic values and pitches for each voice. 

.. code:: common-lisp

    (; rhythmic values of voice 1
     (1/4 1/8 1/8 1/8 1/4 1/8 1/8 1/8 1/8 1/8)
     ; pitches
     (60 61 60 61 61 60 60 60 60 61)
     ; time signatures
     ((3 4) (3 4) (3 4)))

The output is automatically translated into a notated score in PWGL, and can be translated into an OpusModus score with the function ``cluster-engine-score``, which in turn can be translated into a notated score with ``preview-score`` -- both functions provided by the library `tot <https://github.com/tanders/tot>`_.

.. code:: common-lisp

    (:|1| ((q c4 e cs4 c4 cs4 cs4 tie) (e cs4 c4 c4 c4 c4 cs4)))

The above constraint problem does not include any musical rules. Without a detailed discussion, in the following a polyphonic example with a few rules is shown (example 8a translated from the PWGL tutorial into plain Common Lisp). Here, the first rule forces both voices to create 12-tone rows. Rule 2 constrains all harmonic intervals between both voices to consonant intervals or 3, 4, 7, 8 or 9 semitones. Finally, the last rule forbids any voice crossings. A possible result is shown below the code.

.. code:: common-lisp

    (ce::clusterengine 
     12 t nil 
     (append 
      ;; rule 1
      (ce::r-pitches-one-voice #'(lambda (x) 
    			       (not (member (mod (car (last x)) 12)
    					    (mapcar #'(lambda (a) (mod a 12)) (butlast x)))))
    			   '(0 1) :all-pitches)
      ;; rule 2
      (ce::r-pitch-pitch #'(lambda (x) 
    			 (member (mod (ce::apply-minus x) 12) '(3 4 7 8 9)))
    		     '(0 1) '(0) :all :no_grace :pitch)
      ;; rule 3
      (ce::R-pitch-pitch #'(lambda (x) (>= (first x) (second x)) )
    		     '(0 1) '(0) :all :no_grace :pitch)) 
     '((4 4)) 
     '(;; domains of rhythmic values and pitches of voice 1
       ((1/4)) 
       ((60) (61) (62) (63) (64) (65) (66) (67) (68) (69) (70) (71) (72) (73) (74) (75) (76) (77) (78) (79))
       ;; domains of rhythmic values and pitches of voice 2
       ((1/4)) 
       ((60) (61) (62) (63) (64) (65) (66) (67) (68) (69) (70) (71) (72) (73) (74) (75) (76) (77) (78) (79))))

.. figure:: graphics/polyphonic-solution.jpg
    :width: 70%


    Possible result of the constraint problem above


For further information refer to the PWGL tutorial for now. Pretty much all PWGL boxes have their equivalent Common Lisp function/macro with the same arguments. An exception is the function ``clusterengine`` itself, where domains of rhythmic values and pitches are defined in Common Lisp as shown above. 

Cluster Rules
~~~~~~~~~~~~~

The Common Lisp library `Cluster Rules <https://github.com/tanders/cluster-rules>`_ extends Cluster Engine by a collection of predefined musical constraints and that way makes it more easy to use. The equivalent PWGL library is `PWGL Cluster Rules <https://github.com/tanders/pwgl-cluster-rules>`_. 

Test
----

This is a test link to the documentation of function :ref:`R-RHYTHMS-ONE-VOICE`.
