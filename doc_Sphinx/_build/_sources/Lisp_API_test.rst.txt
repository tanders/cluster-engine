Lisp API documentation test
===========================

The following tests report the interface of a Common Lisp function, but using the built-in doc interface for Python functions. It copies the doc-string of the function documentation directly into the RST file.





Test: use a definition list entry for documenting a function -- probably best option.

..  Add an explicit index entry, which is the next section. See file:///Users/torsten/common-lisp/cluster-engine/doc_Sphinx/_build/Lisp_API_test.html
.. index::
   single: clusterengine (definition list entry)

*function* (**clusterengine** no-of-variables rnd? debug? rules metric-domain list-of-domains)

              The Cluster Engine - the main function of this library. 
	      Pitch domains cannot exist without at least one duration in the rhythm domain. Domains with only one value will not use up any time in the search process.
	      See the PWGL tutorials of this library for a detailed discussion of this function.

	      :RND?: (bool) Todo: add param doc here. Todo: add param doc here Todo: add param doc here Todo: add param doc here
	      :DEBUG?: (bool) Todo: add param doc here
	      :RULES:  (list of functions) Todo: add param doc here



This is a test link to :ref:`clusterengine`.


..  Add an explicit index entry, which is the next section. See file:///Users/torsten/common-lisp/cluster-engine/doc_Sphinx/_build/Lisp_API_test.html
.. index::
   single: clusterengine2 (definition list entry)

(**clusterengine** no-of-variables rnd? debug? rules metric-domain list-of-domains)  : *function*

              The Cluster Engine - the main function of this library. 
	      Pitch domains cannot exist without at least one duration in the rhythm domain. Domains with only one value will not use up any time in the search process.
	      See the PWGL tutorials of this library for a detailed discussion of this function.

	      :RND?: (bool) Todo: add param doc here. Todo: add param doc here Todo: add param doc here Todo: add param doc here
	      :DEBUG?: (bool) Todo: add param doc here
	      :RULES:  (list of functions) Todo: add param doc here


		       
				  
Test: use a section for documenting a function -- does that work?
		  
.. index::
   single: package clusterengine3 (section headline)
   
*function* clusterengine
------------------------

              Syntax: ``(clusterengine no-of-variables rnd? debug? rules metric-domain list-of-domains)``

              The Cluster Engine - the main function of this library. 
	      Pitch domains cannot exist without at least one duration in the rhythm domain. Domains with only one value will not use up any time in the search process.
	      See the PWGL tutorials of this library for a detailed discussion of this function.

	      :RND?: (bool) Todo: add param doc here. Todo: add param doc here Todo: add param doc here Todo: add param doc here
	      :DEBUG?: (bool) Todo: add param doc here
	      :RULES:  (list of functions) Todo: add param doc here



Test: use a list entry for documenting a function -- does that work?

.. index::
   single: clusterengine4 (list entry)

- (**clusterengine** no-of-variables rnd? debug? rules metric-domain list-of-domains)

              The Cluster Engine - the main function of this library. 
	      Pitch domains cannot exist without at least one duration in the rhythm domain. Domains with only one value will not use up any time in the search process.
	      See the PWGL tutorials of this library for a detailed discussion of this function.

	      :RND?: (bool) Todo: add param doc here. Todo: add param doc here Todo: add param doc here Todo: add param doc here
	      :DEBUG?: (bool) Todo: add param doc here
	      :RULES:  (list of functions) Todo: add param doc here


				  
.. Adding an index entry inline. This also outputs the actual index entry.
   :index:``test-index-entry``

.. js:function:: (clusterengine no-of-variables rnd? debug? rules metric-domain list-of-domains)
		 
	      The Cluster Engine - the main function of this library. 
	      Pitch domains cannot exist without at least one duration in the rhythm domain. Domains with only one value will not use up any time in the search process.
	      See the PWGL tutorials of this library for a detailed discussion of this function.
		 
.. js:function:: (r-rhythms-one-voice rule voices &optional (rule-type :true/false) (weight 1))

	      We can document Lisp function arguments as in Python documentation.

	      :RULE: is a logic statement in he form of a function. The output of the function has to be either true or false. If there are more than one input to the function, they will receive consecutive durations (or consecutive motifs depending on the input-mode). 
	      :VOICES: is the number for the voice (starting at 0) that the rule affects. It is possible to give a list of several voice numbers: The rule will then be applied to every voice in the list (independant of each other).

	      Now with type information.

	      :param function RULE: is a logic statement in he form of a function. The output of the function has to be either true or false. If there are more than one input to the function, they will receive consecutive durations (or consecutive motifs depending on the input-mode). 
	      :param int-or-list VOICES: is the number for the voice (starting at 0) that the rule affects. It is possible to give a list of several voice numbers: The rule will then be applied to every voice in the list (independant of each other).

	      Note that more complex types are currently difficult to show this way (seemingly needs to be a single word, at least with default settings).

	      **This is a test intermediate headline -- just a bold phrase...**

	      The function doc goes on here...

	      .. code-block:: common-lisp
			      
	          ;; a code block within the function doc -- usage example
	          (list 1 2 3 4)

			      
	      This is still the function documentation. Terminating a ``code-block`` is a bit tricky...


Lisp function documented with JavaScript function domain
	      
.. js:function:: test-function arg1 arg2 arg3
		 
	      This is the documentation of test-function
   

Such Lisp documentation is displayed, but not fully supported. Because they are not recognised as Python function interfaces, they are **not included in the index**, nor are they layouted like Python functions. 


..

   Lets try the Sphinx standard domain instead. It shows that they are no improvement over using the Python domain. The ``program`` directive cannot even contain anything. 


   .. option:: (r-rhythms-one-voice rule voices &optional (rule-type :true/false) (weight 1))

		 :param function RULE: is a logic statement in he form of a function. The output of the function has to be either true or false. If there are more than one input to the function, they will receive consecutive durations (or consecutive motifs depending on the input-mode). 
		 :param int-or-list VOICES: is the number for the voice (starting at 0) that the rule affects. It is possible to give a list of several voice numbers: The rule will then be applied to every voice in the list (independant of each other).

		 Note that more complex types are currently difficult to show this way (seemingly needs to be a single word, at least with default settings).


   .. object:: (r-rhythms-one-voice rule voices &optional (rule-type :true/false) (weight 1))

		 :param function RULE: is a logic statement in he form of a function. The output of the function has to be either true or false. If there are more than one input to the function, they will receive consecutive durations (or consecutive motifs depending on the input-mode). 
		 :param int-or-list VOICES: is the number for the voice (starting at 0) that the rule affects. It is possible to give a list of several voice numbers: The rule will then be applied to every voice in the list (independant of each other).

		 Note that more complex types are currently difficult to show this way (seemingly needs to be a single word, at least with default settings).




