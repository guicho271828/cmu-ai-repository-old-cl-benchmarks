
#include <cmpinclude.h>
#include "frpoly-mod.h"
init_code(start,size,data)char *start;int size;object data;
{	register object *base=vs_top;VC2 register object *sup=base+VM2;vs_top=sup;vs_check;
	Cstart=start;Csize=size;Cdata=data;set_VV(VV,VM1,data);
	VV[0]->s.s_stype=(short)stp_special;
	VV[1]->s.s_stype=(short)stp_special;
	VV[2]->s.s_stype=(short)stp_special;
	VV[3]->s.s_stype=(short)stp_special;
	VV[4]->s.s_stype=(short)stp_special;
	VV[5]->s.s_stype=(short)stp_special;
	VV[6]->s.s_stype=(short)stp_special;
	VV[7]->s.s_stype=(short)stp_special;
	VV[8]->s.s_stype=(short)stp_special;
	VV[9]->s.s_stype=(short)stp_special;
	VV[10]->s.s_stype=(short)stp_special;
	VV[11]->s.s_stype=(short)stp_special;
	VV[12]->s.s_stype=(short)stp_special;
	VV[13]->s.s_stype=(short)stp_special;
	VV[14]->s.s_stype=(short)stp_special;
	VV[15]->s.s_stype=(short)stp_special;
	VV[16]->s.s_stype=(short)stp_special;
	VV[17]->s.s_stype=(short)stp_special;
	VV[18]->s.s_stype=(short)stp_special;
	VV[19]->s.s_stype=(short)stp_special;
	VV[20]->s.s_stype=(short)stp_special;
	VV[21]->s.s_stype=(short)stp_special;
	VV[22]->s.s_stype=(short)stp_special;
	VV[23]->s.s_stype=(short)stp_special;
	VV[24]->s.s_stype=(short)stp_special;
	VV[25]->s.s_stype=(short)stp_special;
	MM(VV[30],L1,start,size,data);
	MM(VV[63],L2,start,size,data);
	MM(VV[64],L3,start,size,data);
	MM(VV[65],L4,start,size,data);
	MM(VV[66],L5,start,size,data);
	MM(VV[67],L6,start,size,data);
	MM(VV[68],L7,start,size,data);
	MM(VV[69],L8,start,size,data);
	MM(VV[70],L9,start,size,data);
	MF(VV[71],L10,start,size,data);
	(void)putprop(VV[71],VV[Vdeb71],VV[72]);
	MF(VV[73],L11,start,size,data);
	(void)putprop(VV[73],VV[Vdeb73],VV[72]);
	MF(VV[74],L12,start,size,data);
	(void)putprop(VV[74],VV[Vdeb74],VV[72]);
	MF(VV[75],L13,start,size,data);
	(void)putprop(VV[75],VV[Vdeb75],VV[72]);
	MF(VV[76],L14,start,size,data);
	(void)putprop(VV[76],VV[Vdeb76],VV[72]);
	MF(VV[77],L15,start,size,data);
	(void)putprop(VV[77],VV[Vdeb77],VV[72]);
	MF(VV[78],L16,start,size,data);
	(void)putprop(VV[78],VV[Vdeb78],VV[72]);
	MF(VV[79],L17,start,size,data);
	(void)putprop(VV[79],VV[Vdeb79],VV[72]);
	MF(VV[80],L18,start,size,data);
	(void)putprop(VV[80],VV[Vdeb80],VV[72]);
	MF(VV[81],L19,start,size,data);
	(void)putprop(VV[81],VV[Vdeb81],VV[72]);
	MF(VV[82],L20,start,size,data);
	(void)putprop(VV[82],VV[Vdeb82],VV[72]);
	MF(VV[83],L21,start,size,data);
	(void)putprop(VV[83],VV[Vdeb83],VV[72]);
	MF(VV[84],L22,start,size,data);
	(void)putprop(VV[84],VV[Vdeb84],VV[72]);
	base[0]= VV[48];
	base[1]= VV[47];
	vs_top=(vs_base=base+0)+2;
	Lset();
	vs_top=sup;
	base[0]= VV[49];
	base[1]= VV[46];
	vs_top=(vs_base=base+0)+2;
	Lset();
	vs_top=sup;
	base[0]= VV[50];
	base[1]= VV[51];
	vs_top=(vs_base=base+0)+2;
	Lset();
	vs_top=sup;
	base[0]= VV[52];
	base[2]= VV[53];
	base[3]= VV[54];
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk77)();
	vs_top=sup;
	base[1]= vs_base[0];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk77)();
	vs_top=sup;
	(VV[19]->s.s_dbind)= vs_base[0];
	base[0]= (VV[19]->s.s_dbind);
	base[1]= VV[55];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk80)();
	vs_top=sup;
	(VV[20]->s.s_dbind)= vs_base[0];
	base[0]= (VV[19]->s.s_dbind);
	base[1]= VV[56];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk80)();
	vs_top=sup;
	(VV[21]->s.s_dbind)= vs_base[0];
	MF(VV[85],L27,start,size,data);
	(void)putprop(VV[85],VV[Vdeb85],VV[72]);
	MF(VV[86],L28,start,size,data);
	(void)putprop(VV[86],VV[Vdeb86],VV[72]);
	MF(VV[87],L29,start,size,data);
	(void)putprop(VV[87],VV[Vdeb87],VV[72]);
	MF(VV[88],L30,start,size,data);
	(void)putprop(VV[88],VV[Vdeb88],VV[72]);
	MF(VV[89],L31,start,size,data);
	(void)putprop(VV[89],VV[Vdeb89],VV[72]);
	MF(VV[90],L32,start,size,data);
	(void)putprop(VV[90],VV[Vdeb90],VV[72]);
	MF(VV[91],L33,start,size,data);
	(void)putprop(VV[91],VV[Vdeb91],VV[72]);
	MF(VV[92],L34,start,size,data);
	(void)putprop(VV[92],VV[Vdeb92],VV[72]);
	MF(VV[93],L35,start,size,data);
	(void)putprop(VV[93],VV[Vdeb93],VV[72]);
	vs_top=vs_base=base;
}
/*	macro definition for VALGET	*/

static L1()
{register object *base=vs_base;
	register object *sup=base+VM3; VC3
	vs_check;
	vs_top=sup;
	{object V10=base[0]->c.c_cdr;
	base[2]= (V10->c.c_car);}
	base[3]= list(3,VV[26],VV[27],list(2,VV[28],base[2]));
	vs_top=(vs_base=base+3)+1;
	return;
}
/*	macro definition for POINTERGP	*/

static L2()
{register object *base=vs_base;
	register object *sup=base+VM4; VC4
	vs_check;
	vs_top=sup;
	{object V11=base[0]->c.c_cdr;
	base[2]= (V11->c.c_car);
	V11=V11->c.c_cdr;
	base[3]= (V11->c.c_car);}
	{object V12= list(2,VV[30],base[2]);
	base[4]= list(3,VV[29],V12,list(2,VV[30],base[3]));
	vs_top=(vs_base=base+4)+1;
	return;}
}
/*	macro definition for F+	*/

static L3()
{register object *base=vs_base;
	register object *sup=base+VM5; VC5
	vs_check;
	vs_top=sup;
	{object V13=base[0]->c.c_cdr;
	base[2]= (V13->c.c_car);
	V13=V13->c.c_cdr;
	base[3]= (V13->c.c_car);}
	{object V14= list(3,VV[26],VV[27],base[2]);
	base[4]= list(3,VV[26],VV[27],list(3,VV[31],V14,list(3,VV[26],VV[27],base[3])));
	vs_top=(vs_base=base+4)+1;
	return;}
}
/*	macro definition for F>	*/

static L4()
{register object *base=vs_base;
	register object *sup=base+VM6; VC6
	vs_check;
	vs_top=sup;
	{object V15=base[0]->c.c_cdr;
	base[2]= (V15->c.c_car);
	V15=V15->c.c_cdr;
	base[3]= (V15->c.c_car);}
	{object V16= list(3,VV[26],VV[27],base[2]);
	base[4]= list(3,VV[29],V16,list(3,VV[26],VV[27],base[3]));
	vs_top=(vs_base=base+4)+1;
	return;}
}
/*	macro definition for PCOEFP	*/

static L5()
{register object *base=vs_base;
	register object *sup=base+VM7; VC7
	vs_check;
	vs_top=sup;
	{object V17=base[0]->c.c_cdr;
	base[2]= (V17->c.c_car);}
	base[3]= list(2,VV[32],base[2]);
	vs_top=(vs_base=base+3)+1;
	return;
}
/*	macro definition for PZEROP	*/

static L6()
{register object *base=vs_base;
	register object *sup=base+VM8; VC8
	vs_check;
	vs_top=sup;
	{object V18=base[0]->c.c_cdr;
	base[2]= (V18->c.c_car);}
	{object V19= list(2,VV[34],list(2,VV[35],base[2]));
	{object V20= list(3,VV[37],base[2],VV[38]);
	{object V21= list(3,VV[39],VV[40],list(3,VV[26],VV[27],base[2]));
	{object V22= list(3,VV[37],base[2],VV[41]);
	base[3]= list(3,VV[33],V19,list(4,VV[36],V20,V21,list(3,VV[36],V22,list(3,VV[42],VV[43],list(3,VV[26],VV[44],base[2])))));
	vs_top=(vs_base=base+3)+1;
	return;}}}}
}
/*	macro definition for PZERO	*/

static L7()
{register object *base=vs_base;
	register object *sup=base+VM9; VC9
	vs_check;
	vs_top=sup;
	{object V23=base[0]->c.c_cdr;}
	base[2]= VV[40];
	vs_top=(vs_base=base+2)+1;
	return;
}
/*	macro definition for CPLUS	*/

static L8()
{register object *base=vs_base;
	register object *sup=base+VM10; VC10
	vs_check;
	vs_top=sup;
	{object V24=base[0]->c.c_cdr;
	base[2]= (V24->c.c_car);
	V24=V24->c.c_cdr;
	base[3]= (V24->c.c_car);}
	base[4]= list(3,VV[31],base[2],base[3]);
	vs_top=(vs_base=base+4)+1;
	return;
}
/*	macro definition for CTIMES	*/

static L9()
{register object *base=vs_base;
	register object *sup=base+VM11; VC11
	vs_check;
	vs_top=sup;
	{object V25=base[0]->c.c_cdr;
	base[2]= (V25->c.c_car);
	V25=V25->c.c_cdr;
	base[3]= (V25->c.c_car);}
	base[4]= list(3,VV[45],base[2],base[3]);
	vs_top=(vs_base=base+4)+1;
	return;
}
/*	function definition for PCOEFADD	*/

static L10()
{register object *base=vs_base;
	register object *sup=base+VM12; VC12
	vs_check;
	{object V26;
	register object V27;
	object V28;
	V26=(base[0]);
	V27=(base[1]);
	V28=(base[2]);
	vs_top=sup;
TTL:;
	if(((type_of((V27))==t_cons?Ct:Cnil))==Cnil){
	goto T27;}
	goto T25;
T27:;
	if(!(type_of((V27))==t_fixnum)){
	goto T30;}
	if(!((0)==(fix((V27))))){
	goto T25;}
	goto T26;
T30:;
	if(!(type_of((V27))==t_shortfloat||type_of((V27))==t_longfloat)){
	goto T33;}
	if(!(number_compare(VV[43],(V27))==0)){
	goto T25;}
	goto T26;
T33:;
	goto T25;
T26:;
	base[3]= (V28);
	vs_top=(vs_base=base+3)+1;
	return;
T25:;
	base[3]= make_cons((V26),make_cons((V27),(V28)));
	vs_top=(vs_base=base+3)+1;
	return;
	}
}
/*	function definition for PCPLUS	*/

static L11()
{register object *base=vs_base;
	register object *sup=base+VM13; VC13
	vs_check;
	{object V29;
	register object V30;
	V29=(base[0]);
	V30=(base[1]);
	vs_top=sup;
TTL:;
	if(!(type_of((V30))!=t_cons)){
	goto T36;}
	base[2]= number_plus((V30),(V29));
	vs_top=(vs_base=base+2)+1;
	return;
T36:;
	base[2]= CMPcar((V30));
	base[4]= (V29);
	base[5]= CMPcdr((V30));
	vs_top=(vs_base=base+4)+2;
	(void) (*Lnk74)();
	vs_top=sup;
	base[3]= vs_base[0];
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk79)();
	return;
	}
}
/*	function definition for PCPLUS1	*/

static L12()
{register object *base=vs_base;
	register object *sup=base+VM14; VC14
	vs_check;
	{register object V31;
	register object V32;
	V31=(base[0]);
	V32=(base[1]);
	vs_top=sup;
TTL:;
	if(((V32))!=Cnil){
	goto T43;}
	if(((type_of((V31))==t_cons?Ct:Cnil))==Cnil){
	goto T48;}
	goto T46;
T48:;
	if(!(type_of((V31))==t_fixnum)){
	goto T51;}
	if(!((0)==(fix((V31))))){
	goto T46;}
	goto T47;
T51:;
	if(!(type_of((V31))==t_shortfloat||type_of((V31))==t_longfloat)){
	goto T54;}
	if(!(number_compare(VV[43],(V31))==0)){
	goto T46;}
	goto T47;
T54:;
	goto T46;
T47:;
	base[2]= Cnil;
	vs_top=(vs_base=base+2)+1;
	return;
T46:;
	base[2]= make_cons(VV[40],make_cons((V31),Cnil));
	vs_top=(vs_base=base+2)+1;
	return;
T43:;
	if(((type_of(CMPcar((V32)))==t_cons?Ct:Cnil))==Cnil){
	goto T59;}
	goto T57;
T59:;
	if(!(type_of(CMPcar((V32)))==t_fixnum)){
	goto T62;}
	if(!((0)==(fix(CMPcar((V32)))))){
	goto T57;}
	goto T58;
T62:;
	if(!(type_of(CMPcar((V32)))==t_shortfloat||type_of(CMPcar((V32)))==t_longfloat)){
	goto T65;}
	if(!(number_compare(VV[43],CMPcar((V32)))==0)){
	goto T57;}
	goto T58;
T65:;
	goto T57;
T58:;
	base[2]= VV[40];
	base[4]= (V31);
	base[5]= CMPcadr((V32));
	vs_top=(vs_base=base+4)+2;
	(void) (*Lnk77)();
	vs_top=sup;
	base[3]= vs_base[0];
	base[4]= Cnil;
	vs_top=(vs_base=base+2)+3;
	(void) (*Lnk71)();
	return;
T57:;
	{object V33= CMPcar((V32));
	{object V34= CMPcadr((V32));
	base[2]= (V31);
	base[3]= CMPcddr((V32));
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk74)();
	vs_top=sup;
	V35= vs_base[0];
	base[2]= make_cons(V33,make_cons(V34,V35));
	vs_top=(vs_base=base+2)+1;
	return;}}
	}
}
/*	function definition for PCTIMES	*/

static L13()
{register object *base=vs_base;
	register object *sup=base+VM15; VC15
	vs_check;
	{object V36;
	register object V37;
	V36=(base[0]);
	V37=(base[1]);
	vs_top=sup;
TTL:;
	if(!(type_of((V37))!=t_cons)){
	goto T76;}
	base[2]= number_times((V36),(V37));
	vs_top=(vs_base=base+2)+1;
	return;
T76:;
	base[2]= CMPcar((V37));
	base[4]= (V36);
	base[5]= CMPcdr((V37));
	vs_top=(vs_base=base+4)+2;
	(void) (*Lnk76)();
	vs_top=sup;
	base[3]= vs_base[0];
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk79)();
	return;
	}
}
/*	function definition for PCTIMES1	*/

static L14()
{register object *base=vs_base;
	register object *sup=base+VM16; VC16
	vs_check;
	{object V38;
	register object V39;
	V38=(base[0]);
	V39=(base[1]);
	vs_top=sup;
TTL:;
	if(((V39))!=Cnil){
	goto T83;}
	base[2]= Cnil;
	vs_top=(vs_base=base+2)+1;
	return;
T83:;
	base[2]= CMPcar((V39));
	base[4]= (V38);
	base[5]= CMPcadr((V39));
	vs_top=(vs_base=base+4)+2;
	(void) (*Lnk80)();
	vs_top=sup;
	base[3]= vs_base[0];
	base[5]= (V38);
	base[6]= CMPcddr((V39));
	vs_top=(vs_base=base+5)+2;
	(void) (*Lnk76)();
	vs_top=sup;
	base[4]= vs_base[0];
	vs_top=(vs_base=base+2)+3;
	(void) (*Lnk71)();
	return;
	}
}
/*	function definition for PPLUS	*/

static L15()
{register object *base=vs_base;
	register object *sup=base+VM17; VC17
	vs_check;
	{register object V40;
	register object V41;
	V40=(base[0]);
	V41=(base[1]);
	vs_top=sup;
TTL:;
	if(!(type_of((V40))!=t_cons)){
	goto T93;}
	base[2]= (V40);
	base[3]= (V41);
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk73)();
	return;
T93:;
	if(!(type_of((V41))!=t_cons)){
	goto T98;}
	base[2]= (V41);
	base[3]= (V40);
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk73)();
	return;
T98:;
	if(!(CMPcar((V40))==CMPcar((V41)))){
	goto T103;}
	base[2]= CMPcar((V40));
	base[4]= CMPcdr((V41));
	base[5]= CMPcdr((V40));
	vs_top=(vs_base=base+4)+2;
	(void) (*Lnk78)();
	vs_top=sup;
	base[3]= vs_base[0];
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk79)();
	return;
T103:;
	base[2]= CMPcar((V40));
	vs_top=(vs_base=base+2)+1;
	Lsymbol_value();
	vs_top=sup;
	V42= vs_base[0];
	base[2]= CMPcar((V41));
	vs_top=(vs_base=base+2)+1;
	Lsymbol_value();
	vs_top=sup;
	V43= vs_base[0];
	if(!((fix(V42))>(fix(V43)))){
	goto T110;}
	base[2]= CMPcar((V40));
	base[4]= (V41);
	base[5]= CMPcdr((V40));
	vs_top=(vs_base=base+4)+2;
	(void) (*Lnk74)();
	vs_top=sup;
	base[3]= vs_base[0];
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk79)();
	return;
T110:;
	base[2]= CMPcar((V41));
	base[4]= (V40);
	base[5]= CMPcdr((V41));
	vs_top=(vs_base=base+4)+2;
	(void) (*Lnk74)();
	vs_top=sup;
	base[3]= vs_base[0];
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk79)();
	return;
	}
}
/*	function definition for PPLUS1	*/

static L16()
{register object *base=vs_base;
	register object *sup=base+VM18; VC18
	vs_check;
	{register object V44;
	register object V45;
	V44=(base[0]);
	V45=(base[1]);
	vs_top=sup;
TTL:;
	if(((V44))!=Cnil){
	goto T125;}
	base[2]= (V45);
	vs_top=(vs_base=base+2)+1;
	return;
T125:;
	if(((V45))!=Cnil){
	goto T128;}
	base[2]= (V44);
	vs_top=(vs_base=base+2)+1;
	return;
T128:;
	if(!(number_compare(CMPcar((V44)),CMPcar((V45)))==0)){
	goto T131;}
	base[2]= CMPcar((V44));
	base[4]= CMPcadr((V44));
	base[5]= CMPcadr((V45));
	vs_top=(vs_base=base+4)+2;
	(void) (*Lnk77)();
	vs_top=sup;
	base[3]= vs_base[0];
	base[5]= CMPcddr((V44));
	base[6]= CMPcddr((V45));
	vs_top=(vs_base=base+5)+2;
	(void) (*Lnk78)();
	vs_top=sup;
	base[4]= vs_base[0];
	vs_top=(vs_base=base+2)+3;
	(void) (*Lnk71)();
	return;
T131:;
	if(!(number_compare(CMPcar((V44)),CMPcar((V45)))>0)){
	goto T141;}
	{object V46= CMPcar((V44));
	{object V47= CMPcadr((V44));
	base[2]= CMPcddr((V44));
	base[3]= (V45);
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk78)();
	vs_top=sup;
	V48= vs_base[0];
	base[2]= make_cons(V46,make_cons(V47,V48));
	vs_top=(vs_base=base+2)+1;
	return;}}
T141:;
	{object V49= CMPcar((V45));
	{object V50= CMPcadr((V45));
	base[2]= (V44);
	base[3]= CMPcddr((V45));
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk78)();
	vs_top=sup;
	V51= vs_base[0];
	base[2]= make_cons(V49,make_cons(V50,V51));
	vs_top=(vs_base=base+2)+1;
	return;}}
	}
}
/*	function definition for PSIMP	*/

static L17()
{register object *base=vs_base;
	register object *sup=base+VM19; VC19
	vs_check;
	{object V52;
	register object V53;
	V52=(base[0]);
	V53=(base[1]);
	vs_top=sup;
TTL:;
	if(((V53))!=Cnil){
	goto T150;}
	base[2]= VV[40];
	vs_top=(vs_base=base+2)+1;
	return;
T150:;
	if(!(type_of((V53))!=t_cons)){
	goto T153;}
	base[2]= (V53);
	vs_top=(vs_base=base+2)+1;
	return;
T153:;
	if(!(number_compare(small_fixnum(0),CMPcar((V53)))==0)){
	goto T156;}
	base[2]= CMPcadr((V53));
	vs_top=(vs_base=base+2)+1;
	return;
T156:;
	base[2]= make_cons((V52),(V53));
	vs_top=(vs_base=base+2)+1;
	return;
	}
}
/*	function definition for PTIMES	*/

static L18()
{register object *base=vs_base;
	register object *sup=base+VM20; VC20
	vs_check;
	{register object V54;
	register object V55;
	V54=(base[0]);
	V55=(base[1]);
	vs_top=sup;
TTL:;
	if(((type_of((V54))==t_cons?Ct:Cnil))==Cnil){
	goto T162;}
	goto T161;
T162:;
	if(!(type_of((V54))==t_fixnum)){
	goto T165;}
	if((0)==(fix((V54)))){
	goto T158;}
	goto T161;
T165:;
	if(!(type_of((V54))==t_shortfloat||type_of((V54))==t_longfloat)){
	goto T168;}
	if(number_compare(VV[43],(V54))==0){
	goto T158;}
	goto T161;
T168:;
T161:;
	if(((type_of((V55))==t_cons?Ct:Cnil))==Cnil){
	goto T171;}
	goto T159;
T171:;
	if(!(type_of((V55))==t_fixnum)){
	goto T174;}
	if(!((0)==(fix((V55))))){
	goto T159;}
	goto T170;
T174:;
	if(!(type_of((V55))==t_shortfloat||type_of((V55))==t_longfloat)){
	goto T177;}
	if(!(number_compare(VV[43],(V55))==0)){
	goto T159;}
	goto T170;
T177:;
	goto T159;
T170:;
T158:;
	base[2]= VV[40];
	vs_top=(vs_base=base+2)+1;
	return;
T159:;
	if(!(type_of((V54))!=t_cons)){
	goto T180;}
	base[2]= (V54);
	base[3]= (V55);
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk75)();
	return;
T180:;
	if(!(type_of((V55))!=t_cons)){
	goto T185;}
	base[2]= (V55);
	base[3]= (V54);
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk75)();
	return;
T185:;
	if(!(CMPcar((V54))==CMPcar((V55)))){
	goto T190;}
	base[2]= CMPcar((V54));
	base[4]= CMPcdr((V54));
	base[5]= CMPcdr((V55));
	vs_top=(vs_base=base+4)+2;
	(void) (*Lnk81)();
	vs_top=sup;
	base[3]= vs_base[0];
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk79)();
	return;
T190:;
	base[2]= CMPcar((V54));
	vs_top=(vs_base=base+2)+1;
	Lsymbol_value();
	vs_top=sup;
	V56= vs_base[0];
	base[2]= CMPcar((V55));
	vs_top=(vs_base=base+2)+1;
	Lsymbol_value();
	vs_top=sup;
	V57= vs_base[0];
	if(!((fix(V56))>(fix(V57)))){
	goto T197;}
	base[2]= CMPcar((V54));
	base[4]= (V55);
	base[5]= CMPcdr((V54));
	vs_top=(vs_base=base+4)+2;
	(void) (*Lnk76)();
	vs_top=sup;
	base[3]= vs_base[0];
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk79)();
	return;
T197:;
	base[2]= CMPcar((V55));
	base[4]= (V54);
	base[5]= CMPcdr((V55));
	vs_top=(vs_base=base+4)+2;
	(void) (*Lnk76)();
	vs_top=sup;
	base[3]= vs_base[0];
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk79)();
	return;
	}
}
/*	function definition for PTIMES1	*/

static L19()
{register object *base=vs_base;
	register object *sup=base+VM21; VC21
	vs_check;
	bds_check;
	{register object V58;
	bds_bind(VV[8],base[0]);
	V58=(base[1]);
	vs_top=sup;
TTL:;
	bds_bind(VV[16],Cnil);
	bds_bind(VV[7],Cnil);
	base[4]= (V58);
	vs_top=(vs_base=base+4)+1;
	(void) (*Lnk82)();
	vs_top=sup;
	(VV[16]->s.s_dbind)= vs_base[0];
	(VV[7]->s.s_dbind)= (VV[16]->s.s_dbind);
T212:;
	(VV[8]->s.s_dbind)= CMPcddr((VV[8]->s.s_dbind));
	if(((VV[8]->s.s_dbind))!=Cnil){
	goto T218;}
	base[4]= (VV[16]->s.s_dbind);
	vs_top=(vs_base=base+4)+1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	return;
T218:;
	base[4]= (V58);
	vs_top=(vs_base=base+4)+1;
	(void) (*Lnk83)();
	vs_top=sup;
	goto T212;
	}
}
/*	function definition for PTIMES2	*/

static L20()
{register object *base=vs_base;
	register object *sup=base+VM22; VC22
	vs_check;
	{register object V59;
	V59=(base[0]);
	vs_top=sup;
TTL:;
	if(((V59))!=Cnil){
	goto T225;}
	base[1]= Cnil;
	vs_top=(vs_base=base+1)+1;
	return;
T225:;
	base[1]= number_plus(CMPcar((VV[8]->s.s_dbind)),CMPcar((V59)));
	base[3]= CMPcadr((VV[8]->s.s_dbind));
	base[4]= CMPcadr((V59));
	vs_top=(vs_base=base+3)+2;
	(void) (*Lnk80)();
	vs_top=sup;
	base[2]= vs_base[0];
	base[4]= CMPcddr((V59));
	vs_top=(vs_base=base+4)+1;
	(void) (*Lnk82)();
	vs_top=sup;
	base[3]= vs_base[0];
	vs_top=(vs_base=base+1)+3;
	(void) (*Lnk71)();
	return;
	}
}
/*	function definition for PTIMES3	*/

static L21()
{register object *base=vs_base;
	register object *sup=base+VM23; VC23
	vs_check;
	{register object V60;
	V60=(base[0]);
	vs_top=sup;
TTL:;
	{register object V61;
	register object V62;
	register object V63;
	V61= Cnil;
	V62= Cnil;
	V63= Cnil;
T234:;
	if(((V60))!=Cnil){
	goto T240;}
	base[1]= Cnil;
	vs_top=(vs_base=base+1)+1;
	return;
T240:;
	V61= CMPmake_fixnum((fix(CMPcar((VV[8]->s.s_dbind))))+(fix(CMPcar((V60)))));
	base[1]= CMPcadr((V60));
	base[2]= CMPcadr((VV[8]->s.s_dbind));
	vs_top=(vs_base=base+1)+2;
	(void) (*Lnk80)();
	vs_top=sup;
	V63= vs_base[0];
	if(((type_of((V63))==t_cons?Ct:Cnil))==Cnil){
	goto T252;}
	goto T250;
T252:;
	if(!(type_of((V63))==t_fixnum)){
	goto T255;}
	if(!((0)==(fix((V63))))){
	goto T250;}
	goto T251;
T255:;
	if(!(type_of((V63))==t_shortfloat||type_of((V63))==t_longfloat)){
	goto T258;}
	if(!(number_compare(VV[43],(V63))==0)){
	goto T250;}
	goto T251;
T258:;
	goto T250;
T251:;
	V60= CMPcddr((V60));
	goto T234;
T250:;
	if(((VV[7]->s.s_dbind))==Cnil){
	goto T262;}
	if(!((fix((V61)))>(fix(CMPcar((VV[7]->s.s_dbind)))))){
	goto T263;}
T262:;
	base[1]= (VV[16]->s.s_dbind);
	base[2]= list(2,(V61),(V63));
	vs_top=(vs_base=base+1)+2;
	(void) (*Lnk78)();
	vs_top=sup;
	(VV[7]->s.s_dbind)= vs_base[0];
	(VV[16]->s.s_dbind)= (VV[7]->s.s_dbind);
	V60= CMPcddr((V60));
	goto T234;
T263:;
	if(!(number_compare((V61),CMPcar((VV[7]->s.s_dbind)))==0)){
	goto T235;}
	base[1]= (V63);
	base[2]= CMPcadr((VV[7]->s.s_dbind));
	vs_top=(vs_base=base+1)+2;
	(void) (*Lnk77)();
	vs_top=sup;
	V63= vs_base[0];
	if(((type_of((V63))==t_cons?Ct:Cnil))==Cnil){
	goto T284;}
	goto T282;
T284:;
	if(!(type_of((V63))==t_fixnum)){
	goto T287;}
	if(!((0)==(fix((V63))))){
	goto T282;}
	goto T283;
T287:;
	if(!(type_of((V63))==t_shortfloat||type_of((V63))==t_longfloat)){
	goto T290;}
	if(!(number_compare(VV[43],(V63))==0)){
	goto T282;}
	goto T283;
T290:;
	goto T282;
T283:;
	base[1]= (VV[16]->s.s_dbind);
	base[2]= list(2,CMPcar((VV[7]->s.s_dbind)),CMPcadr((VV[7]->s.s_dbind)));
	vs_top=(vs_base=base+1)+2;
	(void) (*Lnk105)();
	vs_top=sup;
	(VV[7]->s.s_dbind)= vs_base[0];
	(VV[16]->s.s_dbind)= (VV[7]->s.s_dbind);
	goto T280;
T282:;
	(CMPcdr((VV[7]->s.s_dbind)))->c.c_car = (V63);
T280:;
	V60= CMPcddr((V60));
	goto T234;
T235:;
	if((CMPcddr((VV[7]->s.s_dbind)))==Cnil){
	goto T298;}
	if(!(number_compare(CMPcaddr((VV[7]->s.s_dbind)),(V61))>0)){
	goto T298;}
	(VV[7]->s.s_dbind)= CMPcddr((VV[7]->s.s_dbind));
	goto T235;
T298:;
	V62= CMPcdr((VV[7]->s.s_dbind));
T236:;
	if((CMPcdr((V62)))==Cnil){
	goto T307;}
	if(!(number_compare(CMPcadr((V62)),(V61))<0)){
	goto T306;}
T307:;
	((V62))->c.c_cdr = make_cons((V61),make_cons((V63),CMPcdr((V62))));
	goto T237;
T306:;
	base[1]= CMPcaddr((V62));
	base[2]= (V63);
	vs_top=(vs_base=base+1)+2;
	(void) (*Lnk77)();
	vs_top=sup;
	V63= vs_base[0];
	if(((type_of((V63))==t_cons?Ct:Cnil))==Cnil){
	goto T315;}
	goto T313;
T315:;
	base[1]= CMPcaddr((V62));
	base[2]= (V63);
	vs_top=(vs_base=base+1)+2;
	(void) (*Lnk77)();
	vs_top=sup;
	V63= vs_base[0];
	if(!(type_of((V63))==t_fixnum)){
	goto T321;}
	base[1]= CMPcaddr((V62));
	base[2]= (V63);
	vs_top=(vs_base=base+1)+2;
	(void) (*Lnk77)();
	vs_top=sup;
	V63= vs_base[0];
	if(!((0)==(fix((V63))))){
	goto T313;}
	goto T314;
T321:;
	base[1]= CMPcaddr((V62));
	base[2]= (V63);
	vs_top=(vs_base=base+1)+2;
	(void) (*Lnk77)();
	vs_top=sup;
	V63= vs_base[0];
	if(!(type_of((V63))==t_shortfloat||type_of((V63))==t_longfloat)){
	goto T330;}
	base[1]= CMPcaddr((V62));
	base[2]= (V63);
	vs_top=(vs_base=base+1)+2;
	(void) (*Lnk77)();
	vs_top=sup;
	V63= vs_base[0];
	if(!(number_compare(VV[43],(V63))==0)){
	goto T313;}
	goto T314;
T330:;
	goto T313;
T314:;
	((V62))->c.c_cdr = CMPcdddr((V62));
	goto T238;
T313:;
	(CMPcddr((V62)))->c.c_car = (V63);
T237:;
	V62= CMPcddr((V62));
T238:;
	V60= CMPcddr((V60));
	if(((V60))!=Cnil){
	goto T342;}
	base[1]= Cnil;
	vs_top=(vs_base=base+1)+1;
	return;
T342:;
	V61= CMPmake_fixnum((fix(CMPcar((VV[8]->s.s_dbind))))+(fix(CMPcar((V60)))));
	base[1]= CMPcadr((V60));
	base[2]= CMPcadr((VV[8]->s.s_dbind));
	vs_top=(vs_base=base+1)+2;
	(void) (*Lnk80)();
	vs_top=sup;
	V63= vs_base[0];
T239:;
	if((CMPcdr((V62)))==Cnil){
	goto T350;}
	if(!(number_compare(CMPcadr((V62)),(V61))>0)){
	goto T350;}
	V62= CMPcddr((V62));
	goto T239;
T350:;
	goto T236;}
	}
}
/*	function definition for PEXPTSQ	*/

static L22()
{register object *base=vs_base;
	register object *sup=base+VM24; VC24
	vs_check;
	{register object V64;
	object V65;
	V64=(base[0]);
	V65=(base[1]);
	vs_top=sup;
TTL:;
	{register object V66;
	register object V67;
	base[2]= (V65);
	base[3]= VV[46];
	vs_top=(vs_base=base+2)+2;
	Lfloor();
	vs_top=sup;
	V66= vs_base[0];
	base[2]= (V65);
	vs_top=(vs_base=base+2)+1;
	Loddp();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T363;}
	V67= (V64);
	goto T361;
T363:;
	V67= VV[47];
T361:;
T367:;
	if(!(number_compare(small_fixnum(0),(V66))==0)){
	goto T368;}
	base[2]= (V67);
	vs_top=(vs_base=base+2)+1;
	return;
T368:;
	base[2]= (V64);
	base[3]= (V64);
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk80)();
	vs_top=sup;
	V64= vs_base[0];
	base[2]= (V66);
	vs_top=(vs_base=base+2)+1;
	Loddp();
	vs_top=sup;
	if((vs_base[0])!=Cnil){
	goto T377;}
	goto T376;
T377:;
	base[2]= (V67);
	base[3]= (V64);
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk80)();
	vs_top=sup;
	V67= vs_base[0];
T376:;
	base[2]= (V66);
	base[3]= VV[46];
	vs_top=(vs_base=base+2)+2;
	Lfloor();
	vs_top=sup;
	V66= vs_base[0];
	goto T367;}
	}
}
/*	function definition for STANDARD-FRPOLY-TEST1	*/

static L27()
{register object *base=vs_base;
	register object *sup=base+VM25; VC25
	vs_check;
	vs_top=sup;
TTL:;
	base[0]= (VV[19]->s.s_dbind);
	base[1]= VV[46];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk84)();
	vs_top=sup;
	base[0]= (VV[20]->s.s_dbind);
	base[1]= VV[46];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk84)();
	vs_top=sup;
	base[0]= (VV[21]->s.s_dbind);
	base[1]= VV[46];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk84)();
	vs_top=sup;
	base[0]= Cnil;
	vs_top=(vs_base=base+0)+1;
	return;
}
/*	function definition for STANDARD-FRPOLY-TEST2	*/

static L28()
{register object *base=vs_base;
	register object *sup=base+VM26; VC26
	vs_check;
	vs_top=sup;
TTL:;
	base[0]= (VV[19]->s.s_dbind);
	base[1]= VV[57];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk84)();
	vs_top=sup;
	base[0]= (VV[20]->s.s_dbind);
	base[1]= VV[57];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk84)();
	vs_top=sup;
	base[0]= (VV[21]->s.s_dbind);
	base[1]= VV[57];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk84)();
	vs_top=sup;
	base[0]= Cnil;
	vs_top=(vs_base=base+0)+1;
	return;
}
/*	function definition for STANDARD-FRPOLY-TEST3	*/

static L29()
{register object *base=vs_base;
	register object *sup=base+VM27; VC27
	vs_check;
	vs_top=sup;
TTL:;
	base[0]= (VV[19]->s.s_dbind);
	base[1]= VV[58];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk84)();
	vs_top=sup;
	base[0]= (VV[20]->s.s_dbind);
	base[1]= VV[58];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk84)();
	vs_top=sup;
	base[0]= (VV[21]->s.s_dbind);
	base[1]= VV[58];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk84)();
	vs_top=sup;
	base[0]= Cnil;
	vs_top=(vs_base=base+0)+1;
	return;
}
/*	function definition for STANDARD-FRPOLY-TEST4	*/

static L30()
{register object *base=vs_base;
	register object *sup=base+VM28; VC28
	vs_check;
	vs_top=sup;
TTL:;
	base[0]= (VV[19]->s.s_dbind);
	base[1]= VV[59];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk84)();
	vs_top=sup;
	base[0]= (VV[20]->s.s_dbind);
	base[1]= VV[59];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk84)();
	vs_top=sup;
	base[0]= (VV[21]->s.s_dbind);
	base[1]= VV[59];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk84)();
	vs_top=sup;
	base[0]= Cnil;
	vs_top=(vs_base=base+0)+1;
	return;
}
/*	function definition for TESTFRPOLY	*/

static L31()
{register object *base=vs_base;
	register object *sup=base+VM29; VC29
	vs_check;
	vs_top=sup;
TTL:;
	vs_base=vs_top;
	(void) (*Lnk90)();
	vs_top=sup;
	vs_base=vs_top;
	(void) (*Lnk91)();
	vs_top=sup;
	vs_base=vs_top;
	(void) (*Lnk92)();
	vs_top=sup;
	vs_base=vs_top;
	(void) (*Lnk93)();
	return;
}
/*	function definition for TESTFRPOLY-1	*/

static L32()
{register object *base=vs_base;
	register object *sup=base+VM30; VC30
	vs_check;
	vs_top=sup;
TTL:;
	{object V69;
	object V70;
	object V71;
	object V72;
	object V73;
	V69= Cnil;
	V70= Cnil;
	V71= Cnil;
	V72= Cnil;
	V73= Cnil;
	vs_base=vs_top;
	Lget_internal_real_time();
	vs_top=sup;
	V69= vs_base[0];
	vs_base=vs_top;
	Lget_internal_run_time();
	vs_top=sup;
	V71= vs_base[0];
	vs_base=vs_top;
	(void) (*Lnk85)();
	Llist();
	vs_top=sup;
	V73= vs_base[0];
	vs_base=vs_top;
	Lget_internal_run_time();
	vs_top=sup;
	V72= vs_base[0];
	vs_base=vs_top;
	Lget_internal_real_time();
	vs_top=sup;
	V70= vs_base[0];
	base[0]= (VV[60]->s.s_dbind);
	vs_top=(vs_base=base+0)+1;
	Lfresh_line();
	vs_top=sup;
	base[0]= (VV[60]->s.s_dbind);
	base[1]= VV[61];
	base[3]= number_minus((V70),(V69));
	base[4]= VV[62];
	vs_top=(vs_base=base+3)+2;
	Ldivide();
	vs_top=sup;
	base[2]= vs_base[0];
	base[4]= number_minus((V72),(V71));
	base[5]= VV[62];
	vs_top=(vs_base=base+4)+2;
	Ldivide();
	vs_top=sup;
	base[3]= vs_base[0];
	vs_top=(vs_base=base+0)+4;
	Lformat();
	vs_top=sup;
	base[0]= (V73);
	vs_top=(vs_base=base+0)+1;
	Lvalues_list();
	vs_top=sup;
	V68= vs_base[0];}
	base[0]= print(V68,Cnil);
	vs_top=(vs_base=base+0)+1;
	return;
}
/*	function definition for TESTFRPOLY-2	*/

static L33()
{register object *base=vs_base;
	register object *sup=base+VM31; VC31
	vs_check;
	vs_top=sup;
TTL:;
	{object V75;
	object V76;
	object V77;
	object V78;
	object V79;
	V75= Cnil;
	V76= Cnil;
	V77= Cnil;
	V78= Cnil;
	V79= Cnil;
	vs_base=vs_top;
	Lget_internal_real_time();
	vs_top=sup;
	V75= vs_base[0];
	vs_base=vs_top;
	Lget_internal_run_time();
	vs_top=sup;
	V77= vs_base[0];
	vs_base=vs_top;
	(void) (*Lnk86)();
	Llist();
	vs_top=sup;
	V79= vs_base[0];
	vs_base=vs_top;
	Lget_internal_run_time();
	vs_top=sup;
	V78= vs_base[0];
	vs_base=vs_top;
	Lget_internal_real_time();
	vs_top=sup;
	V76= vs_base[0];
	base[0]= (VV[60]->s.s_dbind);
	vs_top=(vs_base=base+0)+1;
	Lfresh_line();
	vs_top=sup;
	base[0]= (VV[60]->s.s_dbind);
	base[1]= VV[61];
	base[3]= number_minus((V76),(V75));
	base[4]= VV[62];
	vs_top=(vs_base=base+3)+2;
	Ldivide();
	vs_top=sup;
	base[2]= vs_base[0];
	base[4]= number_minus((V78),(V77));
	base[5]= VV[62];
	vs_top=(vs_base=base+4)+2;
	Ldivide();
	vs_top=sup;
	base[3]= vs_base[0];
	vs_top=(vs_base=base+0)+4;
	Lformat();
	vs_top=sup;
	base[0]= (V79);
	vs_top=(vs_base=base+0)+1;
	Lvalues_list();
	vs_top=sup;
	V74= vs_base[0];}
	base[0]= print(V74,Cnil);
	vs_top=(vs_base=base+0)+1;
	return;
}
/*	function definition for TESTFRPOLY-3	*/

static L34()
{register object *base=vs_base;
	register object *sup=base+VM32; VC32
	vs_check;
	vs_top=sup;
TTL:;
	{object V81;
	object V82;
	object V83;
	object V84;
	object V85;
	V81= Cnil;
	V82= Cnil;
	V83= Cnil;
	V84= Cnil;
	V85= Cnil;
	vs_base=vs_top;
	Lget_internal_real_time();
	vs_top=sup;
	V81= vs_base[0];
	vs_base=vs_top;
	Lget_internal_run_time();
	vs_top=sup;
	V83= vs_base[0];
	vs_base=vs_top;
	(void) (*Lnk87)();
	Llist();
	vs_top=sup;
	V85= vs_base[0];
	vs_base=vs_top;
	Lget_internal_run_time();
	vs_top=sup;
	V84= vs_base[0];
	vs_base=vs_top;
	Lget_internal_real_time();
	vs_top=sup;
	V82= vs_base[0];
	base[0]= (VV[60]->s.s_dbind);
	vs_top=(vs_base=base+0)+1;
	Lfresh_line();
	vs_top=sup;
	base[0]= (VV[60]->s.s_dbind);
	base[1]= VV[61];
	base[3]= number_minus((V82),(V81));
	base[4]= VV[62];
	vs_top=(vs_base=base+3)+2;
	Ldivide();
	vs_top=sup;
	base[2]= vs_base[0];
	base[4]= number_minus((V84),(V83));
	base[5]= VV[62];
	vs_top=(vs_base=base+4)+2;
	Ldivide();
	vs_top=sup;
	base[3]= vs_base[0];
	vs_top=(vs_base=base+0)+4;
	Lformat();
	vs_top=sup;
	base[0]= (V85);
	vs_top=(vs_base=base+0)+1;
	Lvalues_list();
	vs_top=sup;
	V80= vs_base[0];}
	base[0]= print(V80,Cnil);
	vs_top=(vs_base=base+0)+1;
	return;
}
/*	function definition for TESTFRPOLY-4	*/

static L35()
{register object *base=vs_base;
	register object *sup=base+VM33; VC33
	vs_check;
	vs_top=sup;
TTL:;
	{object V87;
	object V88;
	object V89;
	object V90;
	object V91;
	V87= Cnil;
	V88= Cnil;
	V89= Cnil;
	V90= Cnil;
	V91= Cnil;
	vs_base=vs_top;
	Lget_internal_real_time();
	vs_top=sup;
	V87= vs_base[0];
	vs_base=vs_top;
	Lget_internal_run_time();
	vs_top=sup;
	V89= vs_base[0];
	vs_base=vs_top;
	(void) (*Lnk88)();
	Llist();
	vs_top=sup;
	V91= vs_base[0];
	vs_base=vs_top;
	Lget_internal_run_time();
	vs_top=sup;
	V90= vs_base[0];
	vs_base=vs_top;
	Lget_internal_real_time();
	vs_top=sup;
	V88= vs_base[0];
	base[0]= (VV[60]->s.s_dbind);
	vs_top=(vs_base=base+0)+1;
	Lfresh_line();
	vs_top=sup;
	base[0]= (VV[60]->s.s_dbind);
	base[1]= VV[61];
	base[3]= number_minus((V88),(V87));
	base[4]= VV[62];
	vs_top=(vs_base=base+3)+2;
	Ldivide();
	vs_top=sup;
	base[2]= vs_base[0];
	base[4]= number_minus((V90),(V89));
	base[5]= VV[62];
	vs_top=(vs_base=base+4)+2;
	Ldivide();
	vs_top=sup;
	base[3]= vs_base[0];
	vs_top=(vs_base=base+0)+4;
	Lformat();
	vs_top=sup;
	base[0]= (V91);
	vs_top=(vs_base=base+0)+1;
	Lvalues_list();
	vs_top=sup;
	V86= vs_base[0];}
	base[0]= print(V86,Cnil);
	vs_top=(vs_base=base+0)+1;
	return;
}
static LnkT88(){ call_or_link(VV[88],&Lnk88);}
static LnkT87(){ call_or_link(VV[87],&Lnk87);}
static LnkT86(){ call_or_link(VV[86],&Lnk86);}
static LnkT85(){ call_or_link(VV[85],&Lnk85);}
static LnkT93(){ call_or_link(VV[93],&Lnk93);}
static LnkT92(){ call_or_link(VV[92],&Lnk92);}
static LnkT91(){ call_or_link(VV[91],&Lnk91);}
static LnkT90(){ call_or_link(VV[90],&Lnk90);}
static LnkT84(){ call_or_link(VV[84],&Lnk84);}
static LnkT105(){ call_or_link(VV[105],&Lnk105);}
static LnkT83(){ call_or_link(VV[83],&Lnk83);}
static LnkT82(){ call_or_link(VV[82],&Lnk82);}
static LnkT81(){ call_or_link(VV[81],&Lnk81);}
static LnkT75(){ call_or_link(VV[75],&Lnk75);}
static LnkT78(){ call_or_link(VV[78],&Lnk78);}
static LnkT73(){ call_or_link(VV[73],&Lnk73);}
static LnkT76(){ call_or_link(VV[76],&Lnk76);}
static LnkT71(){ call_or_link(VV[71],&Lnk71);}
static LnkT79(){ call_or_link(VV[79],&Lnk79);}
static LnkT74(){ call_or_link(VV[74],&Lnk74);}
static LnkT80(){ call_or_link(VV[80],&Lnk80);}
static LnkT77(){ call_or_link(VV[77],&Lnk77);}
