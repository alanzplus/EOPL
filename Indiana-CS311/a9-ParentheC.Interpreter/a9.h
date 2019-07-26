void *exprr__ex__, *addressr__ex__, *envr__ex__, *contr__ex__, *vr__ex__,
  *closurer__ex__;

void (*pc) ();

struct expr;
typedef struct expr expr;
struct expr
{
  enum
  {
    _const_expr,
    _var_expr,
    _if_expr,
    _mult_expr,
    _subr1_expr,
    _zero_expr,
    _letcc_expr,
    _throw_expr,
    _let_expr,
    _lambda_expr,
    _app_expr
  } tag;
  union
  {
    struct
    {
      void *_cexp;
    } _const;
    struct
    {
      void *_n;
    } _var;
    struct
    {
      void *_test;
      void *_conseq;
      void *_alt;
    } _if;
    struct
    {
      void *_nexpr1;
      void *_nexpr2;
    } _mult;
    struct
    {
      void *_nexp;
    } _subr1;
    struct
    {
      void *_nexp;
    } _zero;
    struct
    {
      void *_body;
    } _letcc;
    struct
    {
      void *_kexp;
      void *_vexp;
    } _throw;
    struct
    {
      void *_exp;
      void *_body;
    } _let;
    struct
    {
      void *_body;
    } _lambda;
    struct
    {
      void *_rator;
      void *_rand;
    } _app;
  } u;
};

void *exprr_const (void *cexp);
void *exprr_var (void *n);
void *exprr_if (void *test, void *conseq, void *alt);
void *exprr_mult (void *nexpr1, void *nexpr2);
void *exprr_subr1 (void *nexp);
void *exprr_zero (void *nexp);
void *exprr_letcc (void *body);
void *exprr_throw (void *kexp, void *vexp);
void *exprr_let (void *exp, void *body);
void *exprr_lambda (void *body);
void *exprr_app (void *rator, void *rand);

struct env;
typedef struct env env;
struct env
{
  enum
  {
    _empty_env,
    _extend_env
  } tag;
  union
  {
    struct
    {
      char dummy;
    } _empty;
    struct
    {
      void *_val;
      void *_savedr__m__env;
    } _extend;
  } u;
};

void *envr_empty ();
void *envr_extend (void *val, void *savedr__m__env);

void applyr__m__env ();
struct closure;
typedef struct closure closure;
struct closure
{
  enum
  {
    _procedure_closure
  } tag;
  union
  {
    struct
    {
      void *_body;
      void *_env;
    } _procedure;
  } u;
};

void *closurer_procedure (void *body, void *env);

void applyr__m__closure ();
struct kt;
typedef struct kt kt;
struct kt
{
  enum
  {
    _empty_kt,
    _mult_kt,
    _multr__m__inner_kt,
    _subr1_kt,
    _zero_kt,
    _if_kt,
    _ifr__m__conseq_kt,
    _ifr__m__alt_kt,
    _letcc_kt,
    _throw_kt,
    _throwr__m__inner_kt,
    _let_kt,
    _letr__m__inner_kt,
    _app_kt,
    _appr__m__inner_kt,
    _closure_kt
  } tag;
  union
  {
    struct
    {
      void *_dismount;
    } _empty;
    struct
    {
      void *_xr2;
      void *_savedr__m__env;
      void *_savedr__m__cont;
    } _mult;
    struct
    {
      void *_v;
      void *_savedr__m__cont;
    } _multr__m__inner;
    struct
    {
      void *_savedr__m__cont;
    } _subr1;
    struct
    {
      void *_savedr__m__cont;
    } _zero;
    struct
    {
      void *_conseq;
      void *_alt;
      void *_savedr__m__env;
      void *_savedr__m__cont;
    } _if;
    struct
    {
      void *_savedr__m__cont;
    } _ifr__m__conseq;
    struct
    {
      void *_savedr__m__cont;
    } _ifr__m__alt;
    struct
    {
      void *_savedr__m__cont;
    } _letcc;
    struct
    {
      void *_savedr__m__env;
      void *_savedr__m__cont;
      void *_vr__m__exp;
    } _throw;
    struct
    {
      void *_savedr__m__cont;
      void *_v;
    } _throwr__m__inner;
    struct
    {
      void *_savedr__m__env;
      void *_savedr__m__cont;
      void *_body;
    } _let;
    struct
    {
      void *_savedr__m__cont;
    } _letr__m__inner;
    struct
    {
      void *_savedr__m__env;
      void *_savedr__m__cont;
      void *_rand;
    } _app;
    struct
    {
      void *_v;
      void *_savedr__m__cont;
    } _appr__m__inner;
    struct
    {
      void *_savedr__m__cont;
    } _closure;
  } u;
};

void *ktr_empty (void *dismount);
void *ktr_mult (void *xr2, void *savedr__m__env, void *savedr__m__cont);
void *ktr_multr__m__inner (void *v, void *savedr__m__cont);
void *ktr_subr1 (void *savedr__m__cont);
void *ktr_zero (void *savedr__m__cont);
void *ktr_if (void *conseq, void *alt, void *savedr__m__env,
	      void *savedr__m__cont);
void *ktr_ifr__m__conseq (void *savedr__m__cont);
void *ktr_ifr__m__alt (void *savedr__m__cont);
void *ktr_letcc (void *savedr__m__cont);
void *ktr_throw (void *savedr__m__env, void *savedr__m__cont,
		 void *vr__m__exp);
void *ktr_throwr__m__inner (void *savedr__m__cont, void *v);
void *ktr_let (void *savedr__m__env, void *savedr__m__cont, void *body);
void *ktr_letr__m__inner (void *savedr__m__cont);
void *ktr_app (void *savedr__m__env, void *savedr__m__cont, void *rand);
void *ktr_appr__m__inner (void *v, void *savedr__m__cont);
void *ktr_closure (void *savedr__m__cont);

void applyr__m__k ();
void valuer__m__ofr__m__cps ();
int main ();
int mount_tram ();

struct _trstr;
typedef struct _trstr _trstr;
struct _trstr
{
  jmp_buf *jmpbuf;
  int value;
};
