Error handling
==============

Most IMAS-Fortran Access Layer procedures return an integer status through an
argument named ``status`` or ``retstatus``. A value of ``0`` indicates success,
while a negative value indicates failure. When a procedure also provides a
``retmesg`` argument, inspect that message for details about the failure.

The complete list and meaning of Access Layer status codes is maintained in the
`IMAS-Core error handling documentation
<https://imas-core.readthedocs.io/en/latest/user_guide/error_handling.html>`__.
