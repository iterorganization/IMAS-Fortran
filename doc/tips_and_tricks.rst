.. _tips_and_tricks:

Tips and Tricks
===============

This page collects useful tips and common patterns when working with the IMAS
Fortran High Level Interface.


Using shared data structures between IDSes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here we will take example of two IDSes that share a common field, for example the ``ggd`` field in the ``profiles`` IDS.
`edge_profiles%ggd <https://imas-data-dictionary.readthedocs.io/en/latest/generated/ids/edge_profiles.html#edge_profiles-ggd>`_ and `plasma_profiles%ggd <https://imas-data-dictionary.readthedocs.io/en/latest/generated/ids/plasma_profiles.html#plasma_profiles-ggd>`_

When you write ids_edge_profiles%ggd(t) = ids_plasma_profiles%ggd(t), Fortran does not copy the data, 
but it copies the memory address. Both structs now point to the same heap block.

This becomes a problem when both IDSes are deallocated: Fortran can keep dangling pointers marked as associated.
IMAS-Core may then free the same memory twice, causing a segmentation fault (SIGSEGV).

So the fix is before deallocating the non-owner, nullify its shared pointers, for example:

.. code-block:: fortran

    nullify(edge_profiles%ggd)           ! break the alias
    call ids_deallocate(edge_profiles)   ! skips %ggd — it's null
    call ids_deallocate(plasma_profiles) ! sole owner, frees once

See the :doc:`use_ids` page for more details on validation.


Useful References
-----------------

* :doc:`load_store_ids` — detailed guide on reading and writing IDS
* :doc:`use_ids` — working with IDS fields and validation
* :doc:`imas_uri` — URI format for specifying database entries
