/* Generated by           cobc 3.1.2.0 */
/* Generated from         c:\desenv\COBOL\fopag\FOLHAPAGAMENTO1.cbl */
/* Generated at           dez 04 2022 11:39:46 */
/* GnuCOBOL build date    Aug 31 2022 20:58:34 */
/* GnuCOBOL package date  Dec 23 2020 12:04:58 UTC */
/* Compile command        c:\gc312vbi\bin\cobc -x -std=ibm -Wall -I$(workspaceFolder} -IC:\desenv\COBOL\fopag\CopyBooks -IC:\desenv\COBOL\fopag\CopyBooks\Public -tFOLHAPAGAMENTO1.LST -v -g -Xref -ftsymbols c:\desenv\COBOL\fopag\FOLHAPAGAMENTO1.cbl */

/* Program local variables for 'FOLHAPAGAMENTO1' */

/* Module initialization indicator */
static unsigned int	initialized = 0;

/* Module structure pointer */
static cob_module	*module = NULL;

/* Global variable pointer */
cob_global		*cob_glob_ptr;


/* File APONTAMENTOS */
static cob_file		*h_APONTAMENTOS = NULL;
static unsigned char	h_APONTAMENTOS_status[4];

/* File RELATORIOFOLHA */
static cob_file		*h_RELATORIOFOLHA = NULL;
static unsigned char	h_RELATORIOFOLHA_status[4];

/* File RELATORIOREJ */
static cob_file		*h_RELATORIOREJ = NULL;
static unsigned char	h_RELATORIOREJ_status[4];

/* Call parameters */
cob_field		*cob_procedure_params[1];

/* Perform frame stack */
struct cob_frame	*temp_index;
struct cob_frame	*frame_overflow;
struct cob_frame	*frame_ptr;
struct cob_frame	frame_stack[255];


/* Data storage */
static int	b_2;	/* RETURN-CODE */
static cob_u8_t	b_18[68] __attribute__((aligned));	/* APONTAMENTOS Record */
static cob_u8_t	b_110[769] __attribute__((aligned));	/* RELATORIOFOLHA Record */
static cob_u8_t	b_112[132] __attribute__((aligned));	/* RELATORIOREJ Record */
static cob_u8_t	b_113[3] __attribute__((aligned));	/* NUM-VERSAO */
static cob_u8_t	b_114[1] __attribute__((aligned));	/* IND-FIM-APONTAMENTO */
static cob_u8_t	b_116[8] __attribute__((aligned));	/* DATA-CORRENTE */
static cob_u8_t	b_121[8] __attribute__((aligned));	/* HORA-CORRENTE */
static cob_u8_t	b_127[9] __attribute__((aligned));	/* CNT-APONTAMENTOS */

/* End of local data storage */


/* Fields (local) */
static cob_field f_9	= {6, b_18, &a_4};	/* NR-MATRICULA */
static cob_field f_18	= {68, b_18, &a_4};	/* APONTAMENTOS Record */
static cob_field f_110	= {769, b_110, &a_4};	/* RELATORIOFOLHA Record */
static cob_field f_112	= {132, b_112, &a_4};	/* RELATORIOREJ Record */
static cob_field f_113	= {3, b_113, &a_2};	/* NUM-VERSAO */
static cob_field f_116	= {8, b_116, &a_3};	/* DATA-CORRENTE */
static cob_field f_121	= {8, b_121, &a_4};	/* HORA-CORRENTE */
static cob_field f_127	= {9, b_127, &a_5};	/* CNT-APONTAMENTOS */

/* End of fields */

