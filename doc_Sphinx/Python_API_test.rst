Python API documentation test
======================

This is some test how to document Common Lisp functions using plain-vanilla Sphinx, i.e., without any Common Lisp specific extensions.

The first test is a real Python function. It only reports the function interface and does not extract any doc string.

.. py:function:: Timer.repeat(repeat=3, number=1000000)

		 This documentation is written explicitly into the doc source.

		 :repeat: This is a description of the first argument
		 :number: This describes the second argument

		 We can also include type information in the argument documentation like so.

		 :param int repeat: This is a description of the first argument
		 :param int number: This describes the second argument
			  

		 The documentation goes on here...

		 
Here is a link to the function above: :py:func:`Timer.repeat`.		 
		 

		 
The next two examples shows some documentation automatically extracted from Python docstrings.		 

.. autofunction:: io.open

.. automodule:: io
   :members:
		 
