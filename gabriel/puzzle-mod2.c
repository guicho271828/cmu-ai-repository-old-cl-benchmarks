
#include <cmpinclude.h>
#include "/usr/home/wfs/gabriel/puzzle-mod2.h"
init_code(start,size,data)char *start;int size;object data;
{	register object *base=vs_top;VC2 register object *sup=base+VM2;vs_top=sup;vs_check;
	Cstart=start;Csize=size;Cdata=data;set_VV(VV,VM1,data);
	base[0]= VV[0];
	base[1]= VV[1];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk37)();
	vs_top=sup;
	base[0]= VV[2];
	base[1]= VV[3];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk37)();
	vs_top=sup;
	base[0]= VV[4];
	base[1]= VV[5];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk37)();
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
	(void) (*Lnk38)();
	vs_top=sup;
	VV[15]->s.s_stype=(short)stp_special;
	if(VV[15]->s.s_dbind == OBJNULL){
	base[0]= one_plus(VV[3]);
	base[1]= VV[12];
	base[2]= VV[13];
	base[3]= VV[14];
	base[4]= VV[6];
	vs_top=(vs_base=base+0)+5;
	(void) (*Lnk39)();
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
	(void) (*Lnk39)();
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
	(void) (*Lnk39)();
	vs_top=sup;
	VV[17]->s.s_dbind = vs_base[0];}
	VV[18]->s.s_stype=(short)stp_special;
	if(VV[18]->s.s_dbind == OBJNULL){
	base[0]= one_plus(VV[1]);
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk39)();
	vs_top=sup;
	VV[18]->s.s_dbind = vs_base[0];}
	VV[19]->s.s_stype=(short)stp_special;
	if(VV[19]->s.s_dbind == OBJNULL){
	{object V1= one_plus(VV[5]);
	base[0]= list(2,V1,one_plus(VV[1]));}
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk39)();
	vs_top=sup;
	VV[19]->s.s_dbind = vs_base[0];}
	base[0]= VV[20];
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk38)();
	vs_top=sup;
	MM(VV[40],L9,start,size,data);
	base[0]= VV[23];
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk38)();
	vs_top=sup;
	base[0]= VV[24];
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk38)();
	vs_top=sup;
	MF(VV[41],L12,start,size,data);
	(void)putprop(VV[41],VV[Vdeb41],VV[42]);
	base[0]= VV[25];
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk38)();
	vs_top=sup;
	base[0]= VV[26];
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk38)();
	vs_top=sup;
	MF(VV[43],L15,start,size,data);
	(void)putprop(VV[43],VV[Vdeb43],VV[42]);
	MF(VV[44],L16,start,size,data);
	(void)putprop(VV[44],VV[Vdeb44],VV[42]);
	MF(VV[45],L17,start,size,data);
	(void)putprop(VV[45],VV[Vdeb45],VV[42]);
	MF(VV[46],L18,start,size,data);
	(void)putprop(VV[46],VV[Vdeb46],VV[42]);
	MF(VV[47],L19,start,size,data);
	(void)putprop(VV[47],VV[Vdeb47],VV[42]);
	MF(VV[48],L20,start,size,data);
	(void)putprop(VV[48],VV[Vdeb48],VV[42]);
	MF(VV[49],L21,start,size,data);
	(void)putprop(VV[49],VV[Vdeb49],VV[42]);
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
	{register int V8;
	V8= 0;
T53:;
	if(!((V8)>(V7))){
	goto T54;}
	VMR4(Ct)
T54:;
	if((((VV[19]->s.s_dbind))->a.a_self[V5*((VV[19]->s.s_dbind))->a.a_dims[1]+V8])==Cnil){
	goto T58;}{object V9;
	V9= (VV[18]->s.s_dbind);
	if(((V9)->v.v_self[(V6)+(V8)])==Cnil){
	goto T58;}}
	VMR4(Cnil)
T58:;
	V8= (V8)+1;
	goto T53;}}
}
/*	function definition for JIL	*/

static L15()
{register object *base=vs_base;
	register object *sup=base+VM5; VC5
	vs_check;
	vs_top=sup;
TTL:;
	base[0]= VV[3];
	vs_top=(vs_base=base+0)+1;
	return;
}
/*	local entry for function PLACE	*/

static int LI16(V12,V13)
register int V12;register int V13;
{	VMB6 VMS6 VMV6
TTL:;
	{int V14;
	V14= ((VV[17]->s.s_dbind))->fixa.fixa_self[V12];
	{register int V15;
	V15= 0;
T70:;
	if(!((V15)>(V14))){
	goto T71;}
	goto T67;
T71:;
	if((((VV[19]->s.s_dbind))->a.a_self[V12*((VV[19]->s.s_dbind))->a.a_dims[1]+V15])==Cnil){
	goto T75;}{object V16;
	V16= (VV[18]->s.s_dbind);
	(void)((V16)->v.v_self[(V13)+(V15)]= Ct);}
T75:;
	V15= (V15)+1;
	goto T70;}
T67:;{object V17;
	V17= (VV[15]->s.s_dbind);
	{int V18= ((VV[16]->s.s_dbind))->fixa.fixa_self[V12];{object V19;
	V19= (VV[15]->s.s_dbind);
	(void)((V17)->fixa.fixa_self[V18]= ((V19)->fixa.fixa_self[((VV[16]->s.s_dbind))->fixa.fixa_self[V12]])-(1));}}}
	{register int V20;
	V20= V13;
T84:;
	if(!((V20)>(511))){
	goto T85;}
	princ_char(10,Cnil);
	princ_str("Puzzle filled",Cnil);
	VMR6(0)
T85:;
	if((((VV[18]->s.s_dbind))->v.v_self[V20])!=Cnil){
	goto T91;}
	VMR6(V20)
T91:;
	V20= (V20)+1;
	goto T84;}}
}
/*	local entry for function PUZZLE-REMOVE	*/

static int LI17(V23,V24)
register int V23;register int V24;
{	VMB7 VMS7 VMV7
TTL:;
	{register int V25;
	V25= ((VV[17]->s.s_dbind))->fixa.fixa_self[V23];
	{register int V26;
	V26= 0;
T101:;
	if(!((V26)>(V25))){
	goto T102;}
	goto T98;
T102:;
	if((((VV[19]->s.s_dbind))->a.a_self[V23*((VV[19]->s.s_dbind))->a.a_dims[1]+V26])==Cnil){
	goto T106;}{object V27;
	V27= (VV[18]->s.s_dbind);
	(void)((V27)->v.v_self[(V24)+(V26)]= Cnil);}
T106:;
	V26= (V26)+1;
	goto T101;}
T98:;{object V28;
	V28= (VV[15]->s.s_dbind);
	{int V29= ((VV[16]->s.s_dbind))->fixa.fixa_self[V23];{object V30;
	V30= (VV[15]->s.s_dbind);
	VMR7((V28)->fixa.fixa_self[V29]= ((V30)->fixa.fixa_self[((VV[16]->s.s_dbind))->fixa.fixa_self[V23]])+(1))}}}}
}
/*	local entry for function TRIAL	*/

static object LI18(V32)
register int V32;
{	VMB8 VMS8 VMV8
TTL:;
	{register int V33;
	V33= 0;
	{register int V34;
	V34= 0;
T115:;
	if(!((V34)>(12))){
	goto T116;}
	(VV[8]->s.s_dbind)= CMPmake_fixnum((fix((VV[8]->s.s_dbind)))+1);
	VMR8(Cnil)
T116:;{object V35;
	V35= (VV[15]->s.s_dbind);
	if(((V35)->fixa.fixa_self[((VV[16]->s.s_dbind))->fixa.fixa_self[V34]])==(0)){
	goto T122;}}
	if((LI12(V34,V32))==Cnil){
	goto T122;}
	V33= LI16(V34,V32);
	if((LI18(V33))!=Cnil){
	goto T129;}
	if(!((V33)==(0))){
	goto T130;}
T129:;
	(VV[8]->s.s_dbind)= CMPmake_fixnum((fix((VV[8]->s.s_dbind)))+(1));
	VMR8(Ct)
T130:;
	(void)(LI17(V34,V32));
T122:;
	V34= (V34)+1;
	goto T115;}}
}
/*	local entry for function DEFINEPIECE	*/

static object LI19(V40,V41,V42,V43)
object V40;int V41;int V42;int V43;
{	VMB9 VMS9 VMV9
TTL:;
	{register int V44;
	V44= 0;
	{register int V45;
	V45= 0;
T143:;
	if(!((V45)>(V41))){
	goto T144;}
	goto T140;
T144:;
	{register int V46;
	V46= 0;
T151:;
	if(!((V46)>(V42))){
	goto T152;}
	goto T148;
T152:;
	{register int V47;
	V47= 0;
T159:;
	if(!((V47)>(V43))){
	goto T160;}
	goto T156;
T160:;{object V48;
	V48= (VV[10]->s.s_dbind);
	V44= (V45)+((fix(V48))*((V46)+((fix((VV[10]->s.s_dbind)))*(V47))));}
	(void)(((VV[19]->s.s_dbind))->a.a_self[fix((VV[7]->s.s_dbind))*((VV[19]->s.s_dbind))->a.a_dims[1]+V44]= Ct);
	V47= (V47)+1;
	goto T159;}
T156:;
	V46= (V46)+1;
	goto T151;}
T148:;
	V45= (V45)+1;
	goto T143;}
T140:;
	(void)(((VV[16]->s.s_dbind))->fixa.fixa_self[fix((VV[7]->s.s_dbind))]= fix((V40)));
	(void)(((VV[17]->s.s_dbind))->fixa.fixa_self[fix((VV[7]->s.s_dbind))]= V44);
	if((fix((VV[7]->s.s_dbind)))==(12)){
	goto T179;}
	(VV[7]->s.s_dbind)= CMPmake_fixnum((fix((VV[7]->s.s_dbind)))+(1));
	VMR9((VV[7]->s.s_dbind))
T179:;
	VMR9(Cnil)}
}
/*	function definition for PUZZLE-START	*/

static L20()
{register object *base=vs_base;
	register object *sup=base+VM10; VC10
	vs_check;
	bds_check;
	vs_top=sup;
TTL:;
	{int V49;
	V49= 0;
T185:;
	if(!((V49)>(511))){
	goto T186;}
	goto T182;
T186:;
	(void)(((VV[18]->s.s_dbind))->v.v_self[V49]= Ct);
	V49= (V49)+1;
	goto T185;}
T182:;
	{int V50;
	V50= 1;
T197:;
	if(!((V50)>(5))){
	goto T198;}
	goto T194;
T198:;
	{register int V51;
	V51= 1;
T205:;
	if(!((V51)>(5))){
	goto T206;}
	goto T202;
T206:;
	{register int V52;
	V52= 1;
T213:;
	if(!((V52)>(5))){
	goto T214;}
	goto T210;
T214:;{object V53;
	V53= (VV[18]->s.s_dbind);{object V54;
	V54= CMPmake_fixnum(V50);{object V55;
	V55= (VV[10]->s.s_dbind);{object V56;
	V56= CMPmake_fixnum((fix(V55))*((V51)+((fix((VV[10]->s.s_dbind)))*(V52))));
	(void)(aset1(V53,fix(number_plus(V54,V56)),Cnil));}}}}
	V52= (V52)+1;
	goto T213;}
T210:;
	V51= (V51)+1;
	goto T205;}
T202:;
	V50= (V50)+1;
	goto T197;}
T194:;
	{int V57;
	V57= 0;
T231:;
	if(!((V57)>(12))){
	goto T232;}
	goto T228;
T232:;
	{register int V58;
	V58= 0;
T239:;
	if(!((V58)>(511))){
	goto T240;}
	goto T236;
T240:;
	(void)(((VV[19]->s.s_dbind))->a.a_self[V57*((VV[19]->s.s_dbind))->a.a_dims[1]+V58]= Cnil);
	V58= (V58)+1;
	goto T239;}
T236:;
	V57= (V57)+1;
	goto T231;}
T228:;
	(VV[7]->s.s_dbind)= VV[6];
	(void)(LI19(VV[6],3,1,0));
	(void)(LI19(VV[6],1,0,3));
	(void)(LI19(VV[6],0,3,1));
	(void)(LI19(VV[6],1,3,0));
	(void)(LI19(VV[6],3,0,1));
	(void)(LI19(VV[6],0,1,3));
	(void)(LI19(VV[27],2,0,0));
	(void)(LI19(VV[27],0,2,0));
	(void)(LI19(VV[27],0,0,2));
	(void)(LI19(VV[29],1,1,0));
	(void)(LI19(VV[29],1,0,1));
	(void)(LI19(VV[29],0,1,1));
	(void)(LI19(VV[3],1,1,1));
	(void)(((VV[15]->s.s_dbind))->fixa.fixa_self[0]= 13);
	(void)(((VV[15]->s.s_dbind))->fixa.fixa_self[1]= 3);
	(void)(((VV[15]->s.s_dbind))->fixa.fixa_self[2]= 1);
	(void)(((VV[15]->s.s_dbind))->fixa.fixa_self[3]= 1);
	{int V59;
	int V60;{object V61;
	V61= (VV[10]->s.s_dbind);
	V59= (1)+((fix(V61))*((1)+(fix((VV[10]->s.s_dbind)))));}
	V60= 0;
	bds_bind(VV[8],VV[6]);
	if((LI12(0,V59))==Cnil){
	goto T274;}
	V60= LI16(0,V59);
	goto T272;
T274:;
	base[1]= Ct;
	base[2]= VV[31];
	vs_top=(vs_base=base+1)+2;
	Lformat();
	vs_top=sup;
T272:;
	if((LI18(V60))==Cnil){
	goto T280;}
	base[1]= Ct;
	base[2]= VV[32];
	base[3]= (VV[8]->s.s_dbind);
	vs_top=(vs_base=base+1)+3;
	Lformat();
	bds_unwind1;
	return;
T280:;
	base[1]= Ct;
	base[2]= VV[33];
	vs_top=(vs_base=base+1)+2;
	Lformat();
	bds_unwind1;
	return;}
}
/*	function definition for TESTPUZZLE	*/

static L21()
{register object *base=vs_base;
	register object *sup=base+VM11; VC11
	vs_check;
	vs_top=sup;
TTL:;
	{object V62;
	object V63;
	object V64;
	object V65;
	object V66;
	V62= Cnil;
	V63= Cnil;
	V64= Cnil;
	V65= Cnil;
	V66= Cnil;
	vs_base=vs_top;
	Lget_internal_real_time();
	vs_top=sup;
	V62= vs_base[0];
	vs_base=vs_top;
	Lget_internal_run_time();
	vs_top=sup;
	V64= vs_base[0];
	vs_base=vs_top;
	(void) (*Lnk48)();
	Llist();
	vs_top=sup;
	V66= vs_base[0];
	vs_base=vs_top;
	Lget_internal_run_time();
	vs_top=sup;
	V65= vs_base[0];
	vs_base=vs_top;
	Lget_internal_real_time();
	vs_top=sup;
	V63= vs_base[0];
	base[0]= (VV[34]->s.s_dbind);
	vs_top=(vs_base=base+0)+1;
	Lfresh_line();
	vs_top=sup;
	base[0]= (VV[34]->s.s_dbind);
	base[1]= VV[35];
	base[3]= number_minus((V63),(V62));
	base[4]= VV[36];
	vs_top=(vs_base=base+3)+2;
	Ldivide();
	vs_top=sup;
	base[2]= vs_base[0];
	base[4]= number_minus((V65),(V64));
	base[5]= VV[36];
	vs_top=(vs_base=base+4)+2;
	Ldivide();
	vs_top=sup;
	base[3]= vs_base[0];
	vs_top=(vs_base=base+0)+4;
	Lformat();
	vs_top=sup;
	base[0]= (V66);
	vs_top=(vs_base=base+0)+1;
	Lvalues_list();
	return;}
}
/*	global entry for the function DEFINEPIECE	*/

static L19()
{	register object *base=vs_base;
	base[0]=(LI19((base[0]),fix(base[1]),fix(base[2]),fix(base[3])));
	vs_top=(vs_base=base)+1;
}
/*	global entry for the function TRIAL	*/

static L18()
{	register object *base=vs_base;
	base[0]=(LI18(fix(base[0])));
	vs_top=(vs_base=base)+1;
}
/*	global entry for the function PUZZLE-REMOVE	*/

static L17()
{	register object *base=vs_base;
	base[0]=CMPmake_fixnum(LI17(fix(base[0]),fix(base[1])));
	vs_top=(vs_base=base)+1;
}
/*	global entry for the function PLACE	*/

static L16()
{	register object *base=vs_base;
	base[0]=CMPmake_fixnum(LI16(fix(base[0]),fix(base[1])));
	vs_top=(vs_base=base)+1;
}
/*	global entry for the function FIT	*/

static L12()
{	register object *base=vs_base;
	base[0]=(LI12(fix(base[0]),fix(base[1])));
	vs_top=(vs_base=base)+1;
}
static LnkT48(){ call_or_link(VV[48],&Lnk48);}
static LnkT39(){ call_or_link(VV[39],&Lnk39);}
static LnkT38(){ call_or_link(VV[38],&Lnk38);}
static LnkT37(){ call_or_link(VV[37],&Lnk37);}
