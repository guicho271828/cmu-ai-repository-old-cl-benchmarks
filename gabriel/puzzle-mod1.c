
#include <cmpinclude.h>
#include "puzzle-mod1.h"
init_code(start,size,data)char *start;int size;object data;
{	register object *base=vs_top;VC2 register object *sup=base+VM2;vs_top=sup;vs_check;
	Cstart=start;Csize=size;Cdata=data;set_VV(VV,VM1,data);
	base[0]= VV[0];
	base[1]= VV[1];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk36)();
	vs_top=sup;
	base[0]= VV[2];
	base[1]= VV[3];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk36)();
	vs_top=sup;
	base[0]= VV[4];
	base[1]= VV[5];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk36)();
	vs_top=sup;
	VV[7]->s.s_stype=(short)stp_special;
	if(VV[7]->s.s_dbind == OBJNULL){
	VV[7]->s.s_dbind = VV[6];}
	VV[8]->s.s_stype=(short)stp_special;
	if(VV[8]->s.s_dbind == OBJNULL){
	VV[8]->s.s_dbind = VV[6];}
	VV[10]->s.s_stype=(short)stp_special;
	if(VV[10]->s.s_dbind == OBJNULL){
	VV[10]->s.s_dbind = VV[9];}
	base[0]= VV[11];
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk37)();
	vs_top=sup;
	VV[15]->s.s_stype=(short)stp_special;
	if(VV[15]->s.s_dbind == OBJNULL){
	base[0]= one_plus(VV[3]);
	base[1]= VV[12];
	base[2]= VV[13];
	base[3]= VV[14];
	base[4]= VV[6];
	vs_top=(vs_base=base+0)+5;
	(void) (*Lnk38)();
	vs_top=sup;
	VV[15]->s.s_dbind = vs_base[0];}
	VV[16]->s.s_stype=(short)stp_special;
	if(VV[16]->s.s_dbind == OBJNULL){
	base[0]= one_plus(VV[5]);
	base[1]= VV[12];
	base[2]= VV[13];
	base[3]= VV[14];
	base[4]= VV[6];
	vs_top=(vs_base=base+0)+5;
	(void) (*Lnk38)();
	vs_top=sup;
	VV[16]->s.s_dbind = vs_base[0];}
	VV[17]->s.s_stype=(short)stp_special;
	if(VV[17]->s.s_dbind == OBJNULL){
	base[0]= one_plus(VV[5]);
	base[1]= VV[12];
	base[2]= VV[13];
	base[3]= VV[14];
	base[4]= VV[6];
	vs_top=(vs_base=base+0)+5;
	(void) (*Lnk38)();
	vs_top=sup;
	VV[17]->s.s_dbind = vs_base[0];}
	VV[18]->s.s_stype=(short)stp_special;
	if(VV[18]->s.s_dbind == OBJNULL){
	base[0]= one_plus(VV[1]);
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk38)();
	vs_top=sup;
	VV[18]->s.s_dbind = vs_base[0];}
	VV[19]->s.s_stype=(short)stp_special;
	if(VV[19]->s.s_dbind == OBJNULL){
	{object V1= one_plus(VV[5]);
	base[0]= list(2,V1,one_plus(VV[1]));}
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk38)();
	vs_top=sup;
	VV[19]->s.s_dbind = vs_base[0];}
	base[0]= VV[20];
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk37)();
	vs_top=sup;
	MM(VV[39],L9,start,size,data);
	base[0]= VV[23];
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk37)();
	vs_top=sup;
	base[0]= VV[24];
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk37)();
	vs_top=sup;
	MF(VV[40],L12,start,size,data);
	(void)putprop(VV[40],VV[Vdeb40],VV[41]);
	base[0]= VV[25];
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk37)();
	vs_top=sup;
	MF(VV[42],L14,start,size,data);
	(void)putprop(VV[42],VV[Vdeb42],VV[41]);
	MF(VV[43],L15,start,size,data);
	(void)putprop(VV[43],VV[Vdeb43],VV[41]);
	MF(VV[44],L16,start,size,data);
	(void)putprop(VV[44],VV[Vdeb44],VV[41]);
	MF(VV[45],L17,start,size,data);
	(void)putprop(VV[45],VV[Vdeb45],VV[41]);
	MF(VV[46],L18,start,size,data);
	(void)putprop(VV[46],VV[Vdeb46],VV[41]);
	MF(VV[47],L19,start,size,data);
	(void)putprop(VV[47],VV[Vdeb47],VV[41]);
	vs_top=vs_base=base;
}
/*	macro definition for FREF	*/

static L9()
{register object *base=vs_base;
	register object *sup=base+VM3; VC3
	vs_check;
	vs_top=sup;
	{object V2=base[0]->c.c_cdr;
	base[2]= (V2->c.c_car);
	V2=V2->c.c_cdr;
	base[3]= (V2->c.c_car);}
	base[4]= list(3,VV[21],VV[13],list(3,VV[22],base[2],list(3,VV[21],VV[13],base[3])));
	vs_top=(vs_base=base+4)+1;
	return;
}
/*	local entry for function FIT	*/

static object LI12(V5,V6)
register int V5;register int V6;
{	VMB4 VMS4 VMV4
TTL:;
	{register int V7;
	V7= ((VV[17]->s.s_dbind))->fixa.fixa_self[V5];
	base[0]= (VV[19]->s.s_dbind);
	{register int V8;
	V8= 0;
T51:;
	if(!((V8)>(V7))){
	goto T52;}
	VMR4(Ct)
T52:;
	if(((base[0])->a.a_self[V5*(base[0])->a.a_dims[1]+V8])==Cnil){
	goto T56;}{object V9;
	V9= (VV[18]->s.s_dbind);
	if(((V9)->v.v_self[(V6)+(V8)])==Cnil){
	goto T56;}}
	VMR4(Cnil)
T56:;
	V8= (V8)+1;
	goto T51;}}
}
/*	local entry for function PLACE	*/

static int LI14(V12,V13)
register int V12;register int V13;
{	VMB5 VMS5 VMV5
TTL:;
	{int V14;
	V14= ((VV[17]->s.s_dbind))->fixa.fixa_self[V12];
	{register int V15;
	V15= 0;
T68:;
	if(!((V15)>(V14))){
	goto T69;}
	goto T65;
T69:;
	if((((VV[19]->s.s_dbind))->a.a_self[V12*((VV[19]->s.s_dbind))->a.a_dims[1]+V15])==Cnil){
	goto T73;}{object V16;
	V16= (VV[18]->s.s_dbind);
	(void)((V16)->v.v_self[(V13)+(V15)]= Ct);}
T73:;
	V15= (V15)+1;
	goto T68;}
T65:;
	{object V17;
	object V18;
	object V19;
	V17= (VV[15]->s.s_dbind);
	V18= CMPmake_fixnum(((VV[16]->s.s_dbind))->fixa.fixa_self[V12]);{object V20;
	V20= (VV[15]->s.s_dbind);
	V19= CMPmake_fixnum(((V20)->fixa.fixa_self[((VV[16]->s.s_dbind))->fixa.fixa_self[V12]])-(1));}
	(void)(aset1((V17),fix((V18)),V19));}
	{register int V22;
	V22= V13;
T85:;
	if(!((V22)>(511))){
	goto T86;}
	princ_char(10,Cnil);
	princ_str("Puzzle filled",Cnil);
	VMR5(0)
T86:;
	if((((VV[18]->s.s_dbind))->v.v_self[V22])!=Cnil){
	goto T92;}
	VMR5(V22)
T92:;
	V22= (V22)+1;
	goto T85;}}
}
/*	local entry for function PUZZLE-REMOVE	*/

static object LI15(V26,V27)
register int V26;register int V27;
{	VMB6 VMS6 VMV6
TTL:;
	{register int V28;
	V28= ((VV[17]->s.s_dbind))->fixa.fixa_self[V26];
	{register int V29;
	V29= 0;
T102:;
	if(!((V29)>(V28))){
	goto T103;}
	goto T99;
T103:;
	if((((VV[19]->s.s_dbind))->a.a_self[V26*((VV[19]->s.s_dbind))->a.a_dims[1]+V29])==Cnil){
	goto T107;}{object V30;
	V30= (VV[18]->s.s_dbind);
	(void)((V30)->v.v_self[(V27)+(V29)]= Cnil);}
T107:;
	V29= (V29)+1;
	goto T102;}
T99:;
	{object V31;
	object V32;
	object V33;
	V31= (VV[15]->s.s_dbind);
	V32= CMPmake_fixnum(((VV[16]->s.s_dbind))->fixa.fixa_self[V26]);{object V34;
	V34= (VV[15]->s.s_dbind);
	V33= CMPmake_fixnum(((V34)->fixa.fixa_self[((VV[16]->s.s_dbind))->fixa.fixa_self[V26]])+(1));}
	VMR6(aset1((V31),fix((V32)),V33))}}
}
/*	local entry for function TRIAL	*/

static object LI16(V37)
register int V37;
{	VMB7 VMS7 VMV7
TTL:;
	{register int V38;
	V38= 0;
	{register int V39;
	V39= 0;
T119:;
	if(!((V39)>(12))){
	goto T120;}
	(VV[8]->s.s_dbind)= CMPmake_fixnum((fix((VV[8]->s.s_dbind)))+1);
	VMR7(Cnil)
T120:;{object V40;
	V40= (VV[15]->s.s_dbind);
	if(((V40)->fixa.fixa_self[((VV[16]->s.s_dbind))->fixa.fixa_self[V39]])==(0)){
	goto T126;}}
	if((LI12(V39,V37))==Cnil){
	goto T126;}
	V38= LI14(V39,V37);
	if((LI16(V38))!=Cnil){
	goto T133;}
	if(!((V38)==(0))){
	goto T134;}
T133:;
	(VV[8]->s.s_dbind)= CMPmake_fixnum((fix((VV[8]->s.s_dbind)))+(1));
	VMR7(Ct)
T134:;
	(void)(LI15(V39,V37));
T126:;
	V39= (V39)+1;
	goto T119;}}
}
/*	local entry for function DEFINEPIECE	*/

static object LI17(V45,V46,V47,V48)
object V45;int V46;int V47;int V48;
{	VMB8 VMS8 VMV8
TTL:;
	{register int V49;
	V49= 0;
	{register int V50;
	V50= 0;
T147:;
	if(!((V50)>(V46))){
	goto T148;}
	goto T144;
T148:;
	{register int V51;
	V51= 0;
T155:;
	if(!((V51)>(V47))){
	goto T156;}
	goto T152;
T156:;
	{register int V52;
	V52= 0;
T163:;
	if(!((V52)>(V48))){
	goto T164;}
	goto T160;
T164:;{object V53;
	V53= (VV[10]->s.s_dbind);
	V49= (V50)+((fix(V53))*((V51)+((fix((VV[10]->s.s_dbind)))*(V52))));}
	(void)(((VV[19]->s.s_dbind))->a.a_self[fix((VV[7]->s.s_dbind))*((VV[19]->s.s_dbind))->a.a_dims[1]+V49]= Ct);
	V52= (V52)+1;
	goto T163;}
T160:;
	V51= (V51)+1;
	goto T155;}
T152:;
	V50= (V50)+1;
	goto T147;}
T144:;
	{object V54;
	object V55;
	V54= (VV[16]->s.s_dbind);
	V55= (VV[7]->s.s_dbind);
	(void)(aset1((V54),fix((V55)),V45));}
	{object V58;
	object V59;
	object V60;
	V58= (VV[17]->s.s_dbind);
	V59= (VV[7]->s.s_dbind);
	V60= CMPmake_fixnum(V49);
	(void)(aset1((V58),fix((V59)),V60));}
	if((fix((VV[7]->s.s_dbind)))==(12)){
	goto T188;}
	(VV[7]->s.s_dbind)= CMPmake_fixnum((fix((VV[7]->s.s_dbind)))+(1));
	VMR8((VV[7]->s.s_dbind))
T188:;
	VMR8(Cnil)}
}
/*	function definition for PUZZLE-START	*/

static L18()
{register object *base=vs_base;
	register object *sup=base+VM9; VC9
	vs_check;
	bds_check;
	vs_top=sup;
TTL:;
	{int V62;
	V62= 0;
T194:;
	if(!((V62)>(511))){
	goto T195;}
	goto T191;
T195:;
	(void)(((VV[18]->s.s_dbind))->v.v_self[V62]= Ct);
	V62= (V62)+1;
	goto T194;}
T191:;
	{int V63;
	V63= 1;
T206:;
	if(!((V63)>(5))){
	goto T207;}
	goto T203;
T207:;
	{register int V64;
	V64= 1;
T214:;
	if(!((V64)>(5))){
	goto T215;}
	goto T211;
T215:;
	{register int V65;
	V65= 1;
T222:;
	if(!((V65)>(5))){
	goto T223;}
	goto T219;
T223:;{object V66;
	V66= (VV[18]->s.s_dbind);{object V67;
	V67= CMPmake_fixnum(V63);{object V68;
	V68= (VV[10]->s.s_dbind);{object V69;
	V69= CMPmake_fixnum((fix(V68))*((V64)+((fix((VV[10]->s.s_dbind)))*(V65))));
	(void)(aset1(V66,fix(number_plus(V67,V69)),Cnil));}}}}
	V65= (V65)+1;
	goto T222;}
T219:;
	V64= (V64)+1;
	goto T214;}
T211:;
	V63= (V63)+1;
	goto T206;}
T203:;
	{int V70;
	V70= 0;
T240:;
	if(!((V70)>(12))){
	goto T241;}
	goto T237;
T241:;
	{register int V71;
	V71= 0;
T248:;
	if(!((V71)>(511))){
	goto T249;}
	goto T245;
T249:;
	(void)(((VV[19]->s.s_dbind))->a.a_self[V70*((VV[19]->s.s_dbind))->a.a_dims[1]+V71]= Cnil);
	V71= (V71)+1;
	goto T248;}
T245:;
	V70= (V70)+1;
	goto T240;}
T237:;
	(VV[7]->s.s_dbind)= VV[6];
	(void)(LI17(VV[6],3,1,0));
	(void)(LI17(VV[6],1,0,3));
	(void)(LI17(VV[6],0,3,1));
	(void)(LI17(VV[6],1,3,0));
	(void)(LI17(VV[6],3,0,1));
	(void)(LI17(VV[6],0,1,3));
	(void)(LI17(VV[26],2,0,0));
	(void)(LI17(VV[26],0,2,0));
	(void)(LI17(VV[26],0,0,2));
	(void)(LI17(VV[28],1,1,0));
	(void)(LI17(VV[28],1,0,1));
	(void)(LI17(VV[28],0,1,1));
	(void)(LI17(VV[3],1,1,1));
	{object V72;
	V72= (VV[15]->s.s_dbind);
	(void)(aset1((V72),fix(VV[6]),VV[29]));}
	{object V76;
	V76= (VV[15]->s.s_dbind);
	(void)(aset1((V76),fix(VV[26]),VV[3]));}
	{object V80;
	V80= (VV[15]->s.s_dbind);
	(void)(aset1((V80),fix(VV[28]),VV[26]));}
	{object V84;
	V84= (VV[15]->s.s_dbind);
	(void)(aset1((V84),fix(VV[3]),VV[26]));}
	{int V88;
	int V89;{object V90;
	V90= (VV[10]->s.s_dbind);
	V88= (1)+((fix(V90))*((1)+(fix((VV[10]->s.s_dbind)))));}
	V89= 0;
	bds_bind(VV[8],VV[6]);
	if((LI12(0,V88))==Cnil){
	goto T287;}
	V89= LI14(0,V88);
	goto T285;
T287:;
	base[1]= Ct;
	base[2]= VV[30];
	vs_top=(vs_base=base+1)+2;
	Lformat();
	vs_top=sup;
T285:;
	if((LI16(V89))==Cnil){
	goto T293;}
	base[1]= Ct;
	base[2]= VV[31];
	base[3]= (VV[8]->s.s_dbind);
	vs_top=(vs_base=base+1)+3;
	Lformat();
	bds_unwind1;
	return;
T293:;
	base[1]= Ct;
	base[2]= VV[32];
	vs_top=(vs_base=base+1)+2;
	Lformat();
	bds_unwind1;
	return;}
}
/*	function definition for TESTPUZZLE	*/

static L19()
{register object *base=vs_base;
	register object *sup=base+VM10; VC10
	vs_check;
	vs_top=sup;
TTL:;
	{object V91;
	object V92;
	object V93;
	object V94;
	object V95;
	V91= Cnil;
	V92= Cnil;
	V93= Cnil;
	V94= Cnil;
	V95= Cnil;
	vs_base=vs_top;
	Lget_internal_real_time();
	vs_top=sup;
	V91= vs_base[0];
	vs_base=vs_top;
	Lget_internal_run_time();
	vs_top=sup;
	V93= vs_base[0];
	vs_base=vs_top;
	(void) (*Lnk46)();
	Llist();
	vs_top=sup;
	V95= vs_base[0];
	vs_base=vs_top;
	Lget_internal_run_time();
	vs_top=sup;
	V94= vs_base[0];
	vs_base=vs_top;
	Lget_internal_real_time();
	vs_top=sup;
	V92= vs_base[0];
	base[0]= (VV[33]->s.s_dbind);
	vs_top=(vs_base=base+0)+1;
	Lfresh_line();
	vs_top=sup;
	base[0]= (VV[33]->s.s_dbind);
	base[1]= VV[34];
	base[3]= number_minus((V92),(V91));
	base[4]= VV[35];
	vs_top=(vs_base=base+3)+2;
	Ldivide();
	vs_top=sup;
	base[2]= vs_base[0];
	base[4]= number_minus((V94),(V93));
	base[5]= VV[35];
	vs_top=(vs_base=base+4)+2;
	Ldivide();
	vs_top=sup;
	base[3]= vs_base[0];
	vs_top=(vs_base=base+0)+4;
	Lformat();
	vs_top=sup;
	base[0]= (V95);
	vs_top=(vs_base=base+0)+1;
	Lvalues_list();
	return;}
}
/*	global entry for the function DEFINEPIECE	*/

static L17()
{	register object *base=vs_base;
	base[0]=(LI17((base[0]),fix(base[1]),fix(base[2]),fix(base[3])));
	vs_top=(vs_base=base)+1;
}
/*	global entry for the function TRIAL	*/

static L16()
{	register object *base=vs_base;
	base[0]=(LI16(fix(base[0])));
	vs_top=(vs_base=base)+1;
}
/*	global entry for the function PUZZLE-REMOVE	*/

static L15()
{	register object *base=vs_base;
	base[0]=(LI15(fix(base[0]),fix(base[1])));
	vs_top=(vs_base=base)+1;
}
/*	global entry for the function PLACE	*/

static L14()
{	register object *base=vs_base;
	base[0]=CMPmake_fixnum(LI14(fix(base[0]),fix(base[1])));
	vs_top=(vs_base=base)+1;
}
/*	global entry for the function FIT	*/

static L12()
{	register object *base=vs_base;
	base[0]=(LI12(fix(base[0]),fix(base[1])));
	vs_top=(vs_base=base)+1;
}
static LnkT46(){ call_or_link(VV[46],&Lnk46);}
static LnkT38(){ call_or_link(VV[38],&Lnk38);}
static LnkT37(){ call_or_link(VV[37],&Lnk37);}
static LnkT36(){ call_or_link(VV[36],&Lnk36);}
