#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "a9.h"

void *
ktr_empty (void *dismount)
{
  kt *_data = (kt *) malloc (sizeof (kt));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _empty_kt;
  _data->u._empty._dismount = dismount;
  return (void *) _data;
}

void *
ktr_mult (void *xr2, void *savedr__m__env, void *savedr__m__cont)
{
  kt *_data = (kt *) malloc (sizeof (kt));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _mult_kt;
  _data->u._mult._xr2 = xr2;
  _data->u._mult._savedr__m__env = savedr__m__env;
  _data->u._mult._savedr__m__cont = savedr__m__cont;
  return (void *) _data;
}

void *
ktr_multr__m__inner (void *v, void *savedr__m__cont)
{
  kt *_data = (kt *) malloc (sizeof (kt));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _multr__m__inner_kt;
  _data->u._multr__m__inner._v = v;
  _data->u._multr__m__inner._savedr__m__cont = savedr__m__cont;
  return (void *) _data;
}

void *
ktr_subr1 (void *savedr__m__cont)
{
  kt *_data = (kt *) malloc (sizeof (kt));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _subr1_kt;
  _data->u._subr1._savedr__m__cont = savedr__m__cont;
  return (void *) _data;
}

void *
ktr_zero (void *savedr__m__cont)
{
  kt *_data = (kt *) malloc (sizeof (kt));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _zero_kt;
  _data->u._zero._savedr__m__cont = savedr__m__cont;
  return (void *) _data;
}

void *
ktr_if (void *conseq, void *alt, void *savedr__m__env, void *savedr__m__cont)
{
  kt *_data = (kt *) malloc (sizeof (kt));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _if_kt;
  _data->u._if._conseq = conseq;
  _data->u._if._alt = alt;
  _data->u._if._savedr__m__env = savedr__m__env;
  _data->u._if._savedr__m__cont = savedr__m__cont;
  return (void *) _data;
}

void *
ktr_ifr__m__conseq (void *savedr__m__cont)
{
  kt *_data = (kt *) malloc (sizeof (kt));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _ifr__m__conseq_kt;
  _data->u._ifr__m__conseq._savedr__m__cont = savedr__m__cont;
  return (void *) _data;
}

void *
ktr_ifr__m__alt (void *savedr__m__cont)
{
  kt *_data = (kt *) malloc (sizeof (kt));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _ifr__m__alt_kt;
  _data->u._ifr__m__alt._savedr__m__cont = savedr__m__cont;
  return (void *) _data;
}

void *
ktr_letcc (void *savedr__m__cont)
{
  kt *_data = (kt *) malloc (sizeof (kt));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _letcc_kt;
  _data->u._letcc._savedr__m__cont = savedr__m__cont;
  return (void *) _data;
}

void *
ktr_throw (void *savedr__m__env, void *savedr__m__cont, void *vr__m__exp)
{
  kt *_data = (kt *) malloc (sizeof (kt));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _throw_kt;
  _data->u._throw._savedr__m__env = savedr__m__env;
  _data->u._throw._savedr__m__cont = savedr__m__cont;
  _data->u._throw._vr__m__exp = vr__m__exp;
  return (void *) _data;
}

void *
ktr_throwr__m__inner (void *savedr__m__cont, void *v)
{
  kt *_data = (kt *) malloc (sizeof (kt));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _throwr__m__inner_kt;
  _data->u._throwr__m__inner._savedr__m__cont = savedr__m__cont;
  _data->u._throwr__m__inner._v = v;
  return (void *) _data;
}

void *
ktr_let (void *savedr__m__env, void *savedr__m__cont, void *body)
{
  kt *_data = (kt *) malloc (sizeof (kt));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _let_kt;
  _data->u._let._savedr__m__env = savedr__m__env;
  _data->u._let._savedr__m__cont = savedr__m__cont;
  _data->u._let._body = body;
  return (void *) _data;
}

void *
ktr_letr__m__inner (void *savedr__m__cont)
{
  kt *_data = (kt *) malloc (sizeof (kt));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _letr__m__inner_kt;
  _data->u._letr__m__inner._savedr__m__cont = savedr__m__cont;
  return (void *) _data;
}

void *
ktr_app (void *savedr__m__env, void *savedr__m__cont, void *rand)
{
  kt *_data = (kt *) malloc (sizeof (kt));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _app_kt;
  _data->u._app._savedr__m__env = savedr__m__env;
  _data->u._app._savedr__m__cont = savedr__m__cont;
  _data->u._app._rand = rand;
  return (void *) _data;
}

void *
ktr_appr__m__inner (void *v, void *savedr__m__cont)
{
  kt *_data = (kt *) malloc (sizeof (kt));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _appr__m__inner_kt;
  _data->u._appr__m__inner._v = v;
  _data->u._appr__m__inner._savedr__m__cont = savedr__m__cont;
  return (void *) _data;
}

void *
ktr_closure (void *savedr__m__cont)
{
  kt *_data = (kt *) malloc (sizeof (kt));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _closure_kt;
  _data->u._closure._savedr__m__cont = savedr__m__cont;
  return (void *) _data;
}

void *
closurer_procedure (void *body, void *env)
{
  closure *_data = (closure *) malloc (sizeof (closure));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _procedure_closure;
  _data->u._procedure._body = body;
  _data->u._procedure._env = env;
  return (void *) _data;
}

void *
envr_empty ()
{
  env *_data = (env *) malloc (sizeof (env));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _empty_env;
  return (void *) _data;
}

void *
envr_extend (void *val, void *savedr__m__env)
{
  env *_data = (env *) malloc (sizeof (env));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _extend_env;
  _data->u._extend._val = val;
  _data->u._extend._savedr__m__env = savedr__m__env;
  return (void *) _data;
}

void *
exprr_const (void *cexp)
{
  expr *_data = (expr *) malloc (sizeof (expr));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _const_expr;
  _data->u._const._cexp = cexp;
  return (void *) _data;
}

void *
exprr_var (void *n)
{
  expr *_data = (expr *) malloc (sizeof (expr));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _var_expr;
  _data->u._var._n = n;
  return (void *) _data;
}

void *
exprr_if (void *test, void *conseq, void *alt)
{
  expr *_data = (expr *) malloc (sizeof (expr));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _if_expr;
  _data->u._if._test = test;
  _data->u._if._conseq = conseq;
  _data->u._if._alt = alt;
  return (void *) _data;
}

void *
exprr_mult (void *nexpr1, void *nexpr2)
{
  expr *_data = (expr *) malloc (sizeof (expr));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _mult_expr;
  _data->u._mult._nexpr1 = nexpr1;
  _data->u._mult._nexpr2 = nexpr2;
  return (void *) _data;
}

void *
exprr_subr1 (void *nexp)
{
  expr *_data = (expr *) malloc (sizeof (expr));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _subr1_expr;
  _data->u._subr1._nexp = nexp;
  return (void *) _data;
}

void *
exprr_zero (void *nexp)
{
  expr *_data = (expr *) malloc (sizeof (expr));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _zero_expr;
  _data->u._zero._nexp = nexp;
  return (void *) _data;
}

void *
exprr_letcc (void *body)
{
  expr *_data = (expr *) malloc (sizeof (expr));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _letcc_expr;
  _data->u._letcc._body = body;
  return (void *) _data;
}

void *
exprr_throw (void *kexp, void *vexp)
{
  expr *_data = (expr *) malloc (sizeof (expr));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _throw_expr;
  _data->u._throw._kexp = kexp;
  _data->u._throw._vexp = vexp;
  return (void *) _data;
}

void *
exprr_let (void *exp, void *body)
{
  expr *_data = (expr *) malloc (sizeof (expr));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _let_expr;
  _data->u._let._exp = exp;
  _data->u._let._body = body;
  return (void *) _data;
}

void *
exprr_lambda (void *body)
{
  expr *_data = (expr *) malloc (sizeof (expr));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _lambda_expr;
  _data->u._lambda._body = body;
  return (void *) _data;
}

void *
exprr_app (void *rator, void *rand)
{
  expr *_data = (expr *) malloc (sizeof (expr));
  if (!_data)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  _data->tag = _app_expr;
  _data->u._app._rator = rator;
  _data->u._app._rand = rand;
  return (void *) _data;
}

int
main ()
{
  exprr__ex__ =
    (void *)
    exprr_let (exprr_lambda
	       (exprr_lambda
		(exprr_if
		 (exprr_zero (exprr_var ((void *) 0)),
		  exprr_const ((void *) 1),
		  exprr_mult (exprr_var ((void *) 0),
			      exprr_app (exprr_app
					 (exprr_var ((void *) 1),
					  exprr_var ((void *) 1)),
					 exprr_subr1 (exprr_var
						      ((void *) 0))))))),
	       exprr_mult (exprr_letcc
			   (exprr_app
			    (exprr_app
			     (exprr_var ((void *) 1), exprr_var ((void *) 1)),
			     exprr_throw (exprr_var ((void *) 0),
					  exprr_app (exprr_app
						     (exprr_var ((void *) 1),
						      exprr_var ((void *) 1)),
						     exprr_const ((void *)
								  4))))),
			   exprr_const ((void *) 5)));
  envr__ex__ = (void *) envr_empty ();
  pc = &valuer__m__ofr__m__cps;
  mount_tram ();
  printf ("Answer: %d\n", (int) vr__ex__);
}

void
valuer__m__ofr__m__cps ()
{
  expr *_c = (expr *) exprr__ex__;
  switch (_c->tag)
    {
    case _const_expr:
      {
	void *val = _c->u._const._cexp;
	vr__ex__ = (void *) val;
	pc = &applyr__m__k;
	break;
      }
    case _mult_expr:
      {
	void *xr1 = _c->u._mult._nexpr1;
	void *xr2 = _c->u._mult._nexpr2;
	exprr__ex__ = (void *) xr1;
	contr__ex__ = (void *) ktr_mult (xr2, envr__ex__, contr__ex__);
	pc = &valuer__m__ofr__m__cps;
	break;
      }
    case _subr1_expr:
      {
	void *x = _c->u._subr1._nexp;
	exprr__ex__ = (void *) x;
	contr__ex__ = (void *) ktr_subr1 (contr__ex__);
	pc = &valuer__m__ofr__m__cps;
	break;
      }
    case _zero_expr:
      {
	void *x = _c->u._zero._nexp;
	exprr__ex__ = (void *) x;
	contr__ex__ = (void *) ktr_zero (contr__ex__);
	pc = &valuer__m__ofr__m__cps;
	break;
      }
    case _if_expr:
      {
	void *test = _c->u._if._test;
	void *conseq = _c->u._if._conseq;
	void *alt = _c->u._if._alt;
	exprr__ex__ = (void *) test;
	contr__ex__ = (void *) ktr_if (conseq, alt, envr__ex__, contr__ex__);
	pc = &valuer__m__ofr__m__cps;
	break;
      }
    case _letcc_expr:
      {
	void *body = _c->u._letcc._body;
	exprr__ex__ = (void *) body;
	envr__ex__ = (void *) envr_extend (contr__ex__, envr__ex__);
	contr__ex__ = (void *) ktr_letcc (contr__ex__);
	pc = &valuer__m__ofr__m__cps;
	break;
      }
    case _throw_expr:
      {
	void *kr__m__exp = _c->u._throw._kexp;
	void *vr__m__exp = _c->u._throw._vexp;
	exprr__ex__ = (void *) kr__m__exp;
	contr__ex__ =
	  (void *) ktr_throw (envr__ex__, contr__ex__, vr__m__exp);
	pc = &valuer__m__ofr__m__cps;
	break;
      }
    case _let_expr:
      {
	void *e = _c->u._let._exp;
	void *body = _c->u._let._body;
	exprr__ex__ = (void *) e;
	contr__ex__ = (void *) ktr_let (envr__ex__, contr__ex__, body);
	pc = &valuer__m__ofr__m__cps;
	break;
      }
    case _lambda_expr:
      {
	void *body = _c->u._lambda._body;
	vr__ex__ = (void *) closurer_procedure (body, envr__ex__);
	pc = &applyr__m__k;
	break;
      }
    case _app_expr:
      {
	void *rator = _c->u._app._rator;
	void *rand = _c->u._app._rand;
	exprr__ex__ = (void *) rator;
	contr__ex__ = (void *) ktr_app (envr__ex__, contr__ex__, rand);
	pc = &valuer__m__ofr__m__cps;
	break;
      }
    case _var_expr:
      {
	void *address = _c->u._var._n;
	addressr__ex__ = (void *) address;
	pc = &applyr__m__env;
	break;
      }
    }
}

void
applyr__m__k ()
{
  kt *_c = (kt *) contr__ex__;
  switch (_c->tag)
    {
    case _empty_kt:
      {
	void *dismount = _c->u._empty._dismount;
	_trstr *trstr = (_trstr *) dismount;
	longjmp (*trstr->jmpbuf, 1);
	break;
      }
    case _mult_kt:
      {
	void *xr2 = _c->u._mult._xr2;
	void *savedr__m__env = _c->u._mult._savedr__m__env;
	void *savedr__m__cont = _c->u._mult._savedr__m__cont;
	exprr__ex__ = (void *) xr2;
	envr__ex__ = (void *) savedr__m__env;
	contr__ex__ =
	  (void *) ktr_multr__m__inner (vr__ex__, savedr__m__cont);
	pc = &valuer__m__ofr__m__cps;
	break;
      }
    case _multr__m__inner_kt:
      {
	void *vr1 = _c->u._multr__m__inner._v;
	void *savedr__m__cont = _c->u._multr__m__inner._savedr__m__cont;
	contr__ex__ = (void *) savedr__m__cont;
	vr__ex__ = (void *) (void *) ((int) vr1 * (int) vr__ex__);
	pc = &applyr__m__k;
	break;
      }
    case _subr1_kt:
      {
	void *savedr__m__cont = _c->u._subr1._savedr__m__cont;
	contr__ex__ = (void *) savedr__m__cont;
	vr__ex__ = (void *) (void *) ((int) vr__ex__ - (int) (void *) 1);
	pc = &applyr__m__k;
	break;
      }
    case _zero_kt:
      {
	void *savedr__m__cont = _c->u._zero._savedr__m__cont;
	contr__ex__ = (void *) savedr__m__cont;
	vr__ex__ = (void *) (vr__ex__ == 0);
	pc = &applyr__m__k;
	break;
      }
    case _if_kt:
      {
	void *conseq = _c->u._if._conseq;
	void *alt = _c->u._if._alt;
	void *savedr__m__env = _c->u._if._savedr__m__env;
	void *savedr__m__cont = _c->u._if._savedr__m__cont;
	if (vr__ex__)
	  {
	    exprr__ex__ = (void *) conseq;
	    envr__ex__ = (void *) savedr__m__env;
	    contr__ex__ = (void *) ktr_ifr__m__conseq (savedr__m__cont);
	    pc = &valuer__m__ofr__m__cps;

	  }
	else
	  {
	    exprr__ex__ = (void *) alt;
	    envr__ex__ = (void *) savedr__m__env;
	    contr__ex__ = (void *) ktr_ifr__m__alt (savedr__m__cont);
	    pc = &valuer__m__ofr__m__cps;

	  }
	break;
      }
    case _ifr__m__conseq_kt:
      {
	void *savedr__m__cont = _c->u._ifr__m__conseq._savedr__m__cont;
	contr__ex__ = (void *) savedr__m__cont;
	pc = &applyr__m__k;
	break;
      }
    case _ifr__m__alt_kt:
      {
	void *savedr__m__cont = _c->u._ifr__m__alt._savedr__m__cont;
	contr__ex__ = (void *) savedr__m__cont;
	pc = &applyr__m__k;
	break;
      }
    case _letcc_kt:
      {
	void *savedr__m__cont = _c->u._letcc._savedr__m__cont;
	contr__ex__ = (void *) savedr__m__cont;
	pc = &applyr__m__k;
	break;
      }
    case _throw_kt:
      {
	void *savedr__m__env = _c->u._throw._savedr__m__env;
	void *savedr__m__cont = _c->u._throw._savedr__m__cont;
	void *vr__m__exp = _c->u._throw._vr__m__exp;
	exprr__ex__ = (void *) vr__m__exp;
	envr__ex__ = (void *) savedr__m__env;
	contr__ex__ =
	  (void *) ktr_throwr__m__inner (savedr__m__cont, vr__ex__);
	pc = &valuer__m__ofr__m__cps;
	break;
      }
    case _throwr__m__inner_kt:
      {
	void *savedr__m__cont = _c->u._throwr__m__inner._savedr__m__cont;
	void *vr1 = _c->u._throwr__m__inner._v;
	contr__ex__ = (void *) vr1;
	pc = &applyr__m__k;
	break;
      }
    case _let_kt:
      {
	void *savedr__m__env = _c->u._let._savedr__m__env;
	void *savedr__m__cont = _c->u._let._savedr__m__cont;
	void *body = _c->u._let._body;
	exprr__ex__ = (void *) body;
	envr__ex__ = (void *) envr_extend (vr__ex__, savedr__m__env);
	contr__ex__ = (void *) ktr_letr__m__inner (savedr__m__cont);
	pc = &valuer__m__ofr__m__cps;
	break;
      }
    case _letr__m__inner_kt:
      {
	void *savedr__m__cont = _c->u._letr__m__inner._savedr__m__cont;
	contr__ex__ = (void *) savedr__m__cont;
	pc = &applyr__m__k;
	break;
      }
    case _app_kt:
      {
	void *savedr__m__env = _c->u._app._savedr__m__env;
	void *savedr__m__cont = _c->u._app._savedr__m__cont;
	void *rand = _c->u._app._rand;
	exprr__ex__ = (void *) rand;
	envr__ex__ = (void *) savedr__m__env;
	contr__ex__ = (void *) ktr_appr__m__inner (vr__ex__, savedr__m__cont);
	pc = &valuer__m__ofr__m__cps;
	break;
      }
    case _appr__m__inner_kt:
      {
	void *vr1 = _c->u._appr__m__inner._v;
	void *savedr__m__cont = _c->u._appr__m__inner._savedr__m__cont;
	closurer__ex__ = (void *) vr1;
	contr__ex__ = (void *) savedr__m__cont;
	pc = &applyr__m__closure;
	break;
      }
    case _closure_kt:
      {
	void *savedr__m__cont = _c->u._closure._savedr__m__cont;
	contr__ex__ = (void *) savedr__m__cont;
	pc = &applyr__m__k;
	break;
      }
    }
}

void
applyr__m__closure ()
{
  closure *_c = (closure *) closurer__ex__;
  switch (_c->tag)
    {
    case _procedure_closure:
      {
	void *body = _c->u._procedure._body;
	void *env = _c->u._procedure._env;
	exprr__ex__ = (void *) body;
	envr__ex__ = (void *) envr_extend (vr__ex__, env);
	contr__ex__ = (void *) ktr_closure (contr__ex__);
	pc = &valuer__m__ofr__m__cps;
	break;
      }
    }
}

void
applyr__m__env ()
{
  env *_c = (env *) envr__ex__;
  switch (_c->tag)
    {
    case _empty_env:
      {
	fprintf (stderr, "unbound identifier");
	exit (1);
	break;
      }
    case _extend_env:
      {
	void *val = _c->u._extend._val;
	void *savedr__m__env = _c->u._extend._savedr__m__env;
	if ((addressr__ex__ == 0))
	  {
	    vr__ex__ = (void *) val;
	    pc = &applyr__m__k;

	  }
	else
	  {
	    envr__ex__ = (void *) savedr__m__env;
	    addressr__ex__ = (void *) (void *) ((int) addressr__ex__ - 1);
	    pc = &applyr__m__env;

	  }
	break;
      }
    }
}

int
mount_tram ()
{
  srand (time (NULL));
  jmp_buf jb;
  _trstr trstr;
  void *dismount;
  int _status = setjmp (jb);
  trstr.jmpbuf = &jb;
  dismount = &trstr;
  if (!_status)
    {
      contr__ex__ = (void *) ktr_empty (dismount);
      for (;;)
	{
	  pc ();
	}
    }
  return 0;
}
