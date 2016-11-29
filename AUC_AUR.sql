/*drop table AUCAURtable;
drop table AUCAURtable2;
drop table AUCAURtable1;*/


create volatile table AUCAURtable, no fallback, no log(
	FIS_YR_NBR_MO	SMALLINT, 
	FIS_MO_NBR	BYTEINT, 
	MKT_KEY	INTEGER, 
	CHNL_KEY	INTEGER, 
	BRD_KEY	INTEGER, 
	WJXBFS1	FLOAT)
primary index (FIS_YR_NBR_MO, FIS_MO_NBR, MKT_KEY, CHNL_KEY, BRD_KEY) on commit preserve rows

;insert into AUCAURtable 
select	a12.FIS_YR_NBR  FIS_YR_NBR_MO,
	a12.FIS_MO_NBR  FIS_MO_NBR,
	a11.MKT_KEY  MKT_KEY,
	a11.CHNL_KEY  CHNL_KEY,
	a11.BRD_KEY  BRD_KEY,
	sum(a11.INV_CST_AMT)  WJXBFS1
from	VIEWINV.VICWA_WK_INV_TXN_CNCY_AGG	a11
	join	VIEWFNDT.VFWND_FIS_CAL_WK_TM_ZN_NAM_DIM	a12
	  on 	(a11.INV_TXN_WK_END_DT = a12.WK_END_DT)
where	(a11.BRD_KEY in (1, 2, 3, 4, 5, 7, 8)
 and a11.MKT_KEY in (2, 5, 3, 7, 4, 1)
 and a11.CHNL_KEY in (1, 2, 3)
and a12.FIS_YR_NBR  in ('?My_year')
 and a11.XCHG_RAT_TO_CNCY_CD in ('USD   ')
 and a11.XCHG_RAT_TYP_CD in ('SPR       ')
 and a11.TXN_TYP_CD = 20)
group by	a12.FIS_YR_NBR,
	a12.FIS_MO_NBR,
	a11.MKT_KEY,
	a11.CHNL_KEY,
	a11.BRD_KEY

;create volatile table AUCAURtable1, no fallback, no log(
	FIS_YR_NBR_MO	SMALLINT, 
	FIS_MO_NBR	BYTEINT, 
	MKT_KEY	INTEGER, 
	CHNL_KEY	INTEGER, 
	BRD_KEY	INTEGER, 
	WJXBFS1	FLOAT)
primary index (FIS_YR_NBR_MO, FIS_MO_NBR, MKT_KEY, CHNL_KEY, BRD_KEY) on commit preserve rows

;insert into AUCAURtable1 
select	a13.FIS_YR_NBR  FIS_YR_NBR_MO,
	a13.FIS_MO_NBR  FIS_MO_NBR,
	a11.MKT_KEY  MKT_KEY,
	a12.DD_CHNL_KEY  CHNL_KEY,
	a11.BRD_KEY  BRD_KEY,
	sum(a11.UN_TXN_INV_QTY)  WJXBFS1
from	VIEWINV.VDICW_DLY_INV_TXN_CNCY_FCT	a11
	join	VIEWFNDT.TLCHL_LOC_CHNL_LOOKUP	a12
	  on 	(a11.CHNL_KEY = a12.CHNL_KEY and 
	a11.LOC_KEY = a12.LOC_KEY)
	join	VIEWFNDT.VCDFD_Cal_Day_Flat_Dim	a13
	  on 	(a11.INV_TXN_DT = a13.FIS_CAL_DT)
where	(a11.BRD_KEY in (1, 2, 3, 4, 5, 7, 8)
 and a11.MKT_KEY in (2, 5, 3, 7, 4, 1)
 and a12.DD_CHNL_KEY in (1, 2, 3)
and a12.FIS_YR_NBR  in ('?My_year')
 and a11.XCHG_RAT_TO_CNCY_CD in ('USD   ')
 and a11.XCHG_RAT_TYP_CD in ('SPR       ')
 and a11.TXN_TYP_CD = 20
 and COALESCE(a11.ADJ_CD,'-')  not in ('C'))
group by	a13.FIS_YR_NBR,
	a13.FIS_MO_NBR,
	a11.MKT_KEY,
	a12.DD_CHNL_KEY,
	a11.BRD_KEY

;create volatile table AUCAURtable2, no fallback, no log(
	FIS_YR_NBR_MO	SMALLINT, 
	FIS_MO_NBR	BYTEINT, 
	MKT_KEY	INTEGER, 
	CHNL_KEY	INTEGER, 
	BRD_KEY	INTEGER, 
	WJXBFS1	FLOAT, 
	WJXBFS2	FLOAT, 
	WJXBFS3	FLOAT)
primary index (FIS_YR_NBR_MO, FIS_MO_NBR, MKT_KEY, CHNL_KEY, BRD_KEY) on commit preserve rows

;insert into AUCAURtable2 
select	a13.FIS_YR_NBR  FIS_YR_NBR_MO,
	a13.FIS_MO_NBR  FIS_MO_NBR,
	a11.MKT_KEY  MKT_KEY,
	a12.DD_CHNL_KEY  CHNL_KEY,
	a11.BRD_KEY  BRD_KEY,
	sum(CAST (a11.CONV_NET_DSCT_UN_AMT AS DECIMAL (18,2)))  WJXBFS1,
	sum(a11.LN_TOT_UN_SLS_QTY)  WJXBFS2,
	sum(CAST (a11.CONV_WAC_CST_AMT AS DECIMAL (18,2)))  WJXBFS3
from	VIEWSLS.VSLWA_SLS_LOC_WK_CNCY_CNV_FCT	a11
	join	VIEWFNDT.TLCHL_LOC_CHNL_LOOKUP	a12
	  on 	(a11.CHNL_KEY = a12.CHNL_KEY and 
	a11.CR_LOC_KEY = a12.LOC_KEY)
	join	VIEWFNDT.VFWND_FIS_CAL_WK_TM_ZN_NAM_DIM	a13
	  on 	(a11.TXN_WK_END_DT = a13.WK_END_DT)
where	(a11.BRD_KEY in (1, 2, 3, 4, 5, 7, 8)
 and a11.MKT_KEY in (2, 5, 3, 7, 4, 1)
 and a12.DD_CHNL_KEY in (1, 2, 3)
and a12.FIS_YR_NBR  in ('?My_year')
 and a11.XCHG_RAT_TO_CNCY_CD in ('USD   ')
 and a11.XCHG_RAT_TYP_CD in ('SPR       ')
 and a11.TXN_ITM_TYP_CD = 'M')
group by	a13.FIS_YR_NBR,
	a13.FIS_MO_NBR,
	a11.MKT_KEY,
	a12.DD_CHNL_KEY,
	a11.BRD_KEY

;select	a14.FIS_YR_NBR  FIS_YR_NBR_YR,
	a14.FIS_YR_NBR  FIS_YR_NBR,
	a14.FIS_QTR_NBR  FIS_QTR_NBR,
	max(a18.FIS_QTR_DESC)  FIS_QTR_DESC,
	coalesce(pa11.FIS_YR_NBR_MO, pa12.FIS_YR_NBR_MO, pa13.FIS_YR_NBR_MO)  FIS_YR_NBR_MO,
	coalesce(pa11.FIS_MO_NBR, pa12.FIS_MO_NBR, pa13.FIS_MO_NBR)  FIS_MO_NBR,
	max(a14.FIS_MO_DESC)  FIS_MO_DESC,
	coalesce(pa11.BRD_KEY, pa12.BRD_KEY, pa13.BRD_KEY)  BRD_KEY,
	max(a15.BRD_NM)  BRD_NM,
	coalesce(pa11.MKT_KEY, pa12.MKT_KEY, pa13.MKT_KEY)  MKT_KEY,
	max(a17.MKT_NM)  MKT_DESC,
	coalesce(pa11.CHNL_KEY, pa12.CHNL_KEY, pa13.CHNL_KEY)  CHNL_KEY,
	max(a16.CHNL_NM)  CHNL_NM,
	max(pa11.WJXBFS1)  "Cost Vendor Rects",
	max(pa12.WJXBFS1)  "Unit Vendor Rects",
	max(pa13.WJXBFS1)  "Rev Sales Amt",
	max(pa13.WJXBFS2)  "Rev Sales Units",
	max(pa13.WJXBFS3)  "Cost Sales Amt"
from	AUCAURtable	pa11
	full outer join	AUCAURtable1	pa12
	  on 	(pa11.BRD_KEY = pa12.BRD_KEY and 
	pa11.CHNL_KEY = pa12.CHNL_KEY and 
	pa11.FIS_MO_NBR = pa12.FIS_MO_NBR and 
	pa11.FIS_YR_NBR_MO = pa12.FIS_YR_NBR_MO and 
	pa11.MKT_KEY = pa12.MKT_KEY)
	full outer join	AUCAURtable2	pa13
	  on 	(coalesce(pa11.BRD_KEY, pa12.BRD_KEY) = pa13.BRD_KEY and 
	coalesce(pa11.CHNL_KEY, pa12.CHNL_KEY) = pa13.CHNL_KEY and 
	coalesce(pa11.FIS_MO_NBR, pa12.FIS_MO_NBR) = pa13.FIS_MO_NBR and 
	coalesce(pa11.FIS_YR_NBR_MO, pa12.FIS_YR_NBR_MO) = pa13.FIS_YR_NBR_MO and 
	coalesce(pa11.MKT_KEY, pa12.MKT_KEY) = pa13.MKT_KEY)
	join	(SELECT A.*, FIS_YR_NBR||(FIS_MO_NBR (format '9(2)') (char(2))) as YEAR_MO
FROM VIEWFNDT.TFMCL_FIS_MO_CAL_LOOKUP A
WHERE WK53_LY_YR_IND = 0)	a14
	  on 	(coalesce(pa11.FIS_MO_NBR, pa12.FIS_MO_NBR, pa13.FIS_MO_NBR) = a14.FIS_MO_NBR and 
	coalesce(pa11.FIS_YR_NBR_MO, pa12.FIS_YR_NBR_MO, pa13.FIS_YR_NBR_MO) = a14.FIS_YR_NBR)
	join	VIEWFNDT.TBRDL_BRD_LOOKUP	a15
	  on 	(coalesce(pa11.BRD_KEY, pa12.BRD_KEY, pa13.BRD_KEY) = a15.BRD_KEY)
	join	VIEWFNDT.TCHNL_CHNL_LOOKUP	a16
	  on 	(coalesce(pa11.CHNL_KEY, pa12.CHNL_KEY, pa13.CHNL_KEY) = a16.CHNL_KEY)
	join	VIEWFNDT.TMKTL_MKT_LOOKUP	a17
	  on 	(coalesce(pa11.MKT_KEY, pa12.MKT_KEY, pa13.MKT_KEY) = a17.MKT_KEY)
	join	(SELECT A.*, FIS_YR_NBR||(FIS_QTR_NBR (format '9(2)') (char(2))) as YEAR_QTR
FROM VIEWFNDT.TFQCL_FIS_QTR_CAL_LOOKUP A
WHERE WK53_LY_YR_IND = 0
)	a18
	  on 	(a14.FIS_QTR_NBR = a18.FIS_QTR_NBR and 
	a14.FIS_YR_NBR = a18.FIS_YR_NBR)
group by	a14.FIS_YR_NBR,
	a14.FIS_YR_NBR,
	a14.FIS_QTR_NBR,
	coalesce(pa11.FIS_YR_NBR_MO, pa12.FIS_YR_NBR_MO, pa13.FIS_YR_NBR_MO),
	coalesce(pa11.FIS_MO_NBR, pa12.FIS_MO_NBR, pa13.FIS_MO_NBR),
	coalesce(pa11.BRD_KEY, pa12.BRD_KEY, pa13.BRD_KEY),
	coalesce(pa11.MKT_KEY, pa12.MKT_KEY, pa13.MKT_KEY),
	coalesce(pa11.CHNL_KEY, pa12.CHNL_KEY, pa13.CHNL_KEY)