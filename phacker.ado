*====================================================*
* Ado-file:      phacker 
* Author:        Bryce YU (俞小坤)
* Affiliation:   Renmin University of China
* E-mail:        yu.xiaokun@outlook.com 
* Date:          2023/6/8                                       
*====================================================*

capture program drop tuples
program define tuples
    version 8
    
    syntax anything(id = "list")     ///
    [ ,                              ///
        ASIS /* or */ VARlist        ///
        MAX(numlist max=1 integer>0) ///
        MIN(numlist max=1 integer>0) ///
        CONDitionals(string asis)    ///
        DIsplay                      ///
        noMata                       ///
        noPYthon                     ///
        noSort                       ///
        LMACNAME(name local)         /// not documented
        SEEMATA  /// debug, now ignored; not documented
        *        /// historical options; not documented
    ]
    
    opts_incompatible `asis' `varlist'
    
    if (`"`options'"' == "") local method ncr // the default
    else {
        /* historical method to produce tuples */
        opts_historical method , `options' `mata' `sort'
        if ("`method'" != "noncr") local python nopython
    }
    
    if (c(stata_version) < 16) local python nopython
    if (c(stata_version) < 10) local mata   nomata
    
    if ("`python'" != "nopython") {
        capture python search
        if ( !_rc ) get_pyfilename st_tuples_py
        if (`"`st_tuples_py'"' == "") local python nopython
    }
    
    if ("`asis'" == "") {
        if ("`varlist'" == "") local capture capture
        `capture' unab anything : `macval(anything)'
    }
    
    /*
        we use -macval()- below to refer to -anything- 
        to preserve escaped quotes, etc. 
    */
    tokenize `"`macval(anything)'"'
    local n : word count `macval(anything)'
    
    if (`"`conditionals'"' != "") {
        if (c(stata_version) < 10) {
            display as err "option conditionals() not allowed"
            exit 198
        }
        if ("`method'" == "naive") local NAIVE naive
        if ("`python'" == "nopython") local NOMATA `mata'
        opts_incompatible conditionals() `NAIVE' `NOMATA'
        mata : conditionals_to_rpn("conditionals", `n')
    }
    
    if ("`max'" == "") local max = `n'
    else if (`max' > `n') {
        display as txt "note: maximum reset to number of items " as res `n'
        local max = `n'
    }
    
    if ("`min'" == "") local min = 1
    else if (`min' > `max') {
        display as txt "note: minimum reset to maximum " as res `max'
        local min = `max'
    }
        
    if ("`lmacname'" == "") local lmacname tuple
    else confirm name _n`lmacname's
    
    
        /* produce the tuples using Python */
    if ("`python'" != "nopython") {
        python script "`st_tuples_py'" /// 
            , args(                    ///
                `min'                  ///
                `max'                  ///
                "`conditionals'"       ///
                "`display'"            ///
                "`sort'"               ///
                "`lmacname'"           ///
                `macval(anything)'     ///
            )
        exit // done
    }
    
    
        /* produce the tuples using Mata */
    if ("`mata'" != "nomata") {
        mata : tuples_ado(              ///
            &tuples_`method'(),         ///
            "anything",                 ///
            `n',                        ///
            `min',                      ///
            `max',                      ///
            "conditionals",             ///
            ("`display'" == "display"), ///
            ("`sort'" != "nosort"),     ///
            "`lmacname'"                ///
            )
        exit // done
    }
    
    
        /* produce the tuples using Stata */
    if ("`display'" == "") local continue continue
    
    local N = 2^`n'-1
    local k = 0
    
    if ("`sort'" == "nosort") {
            /* faster variation of original algorithm */
        forvalues i = 1/`N' {
            quietly inbase 2 `i'
            local indicators : display %0`n'.0f `r(base)'
            local one 0
            local tuple // void
            local space // void
            forvalues j = 1/`n' {
                if (substr("`indicators'", `j', 1) == "1") {
                    if (`++one' > `max') continue , break
                    local tuple `"`macval(tuple)'`space'`macval(`j')'"'
                    local space " "
                }
            }
            if ( (`one'<`min') | (`one'>`max') ) continue
            c_local `lmacname'`++k' `"`macval(tuple)'"'
            `continue'
            display as res "`lmacname'`k': " as txt `"`macval(tuple)'"'
        }
    }
    else {
            /* original algorithm */
        forval I = `min'/`max' { 
            forval i = 1/`N' { 
                qui inbase 2 `i'
                local which `r(base)' 
                local nzeros = `n' - `: length local which' 
                local zeros : di _dup(`nzeros') "0" 
                local which `zeros'`which'  
                local which : subinstr local which "1" "1", all count(local n1) 
                if `n1' == `I' {
                    local out   // void
                    local space // void
                    forval j = 1 / `n' { 
                        local char = substr("`which'",`j',1) 
                        if `char' {
                            local out `"`macval(out)'`space'`macval(`j')'"'
                            local space " "
                        }
                    }
                    c_local `lmacname'`++k' `"`macval(out)'"'
                    `continue'
                    display as res "`lmacname'`k': " as txt `"`macval(out)'"'
                }
            }
        }
    }
    
    c_local n`lmacname's `k'
end

program opts_incompatible
    if ( mi("`2'") ) exit
    display as err "options `1' and `2' may not be combined"
    exit 198
end

program opts_historical
    /*
        historical options for selecting the method to produce tuples
        
            - methods are mutually exclusive
            - methods cannot be combined with -nomata-
            - method -naive- must be combined with -nosort-
        
        returns in local macro <namelist> exactly one of
            noncr | ncr | cvp | kronecker | naive
    */
    syntax name(local) [ , NONCR NCR CVP KRONECKER NAIVE noMATA noSort ]
    
    opts_incompatible `noncr' `ncr'
    opts_incompatible `ncr' `cvp' `kronecker' `naive' `mata'
    
    if ("`naive'" != "") & ("`sort'" == "") {
        display as err "option nosort required"
        exit 198
    }
    
    local method `ncr' `cvp' `kronecker' `naive'
    if ("`method'" != "") {
        if (c(stata_version) < 10) {
            display as txt "note: option `method' ignored"
        }
        local noncr // void
    }
    
    c_local `namelist' `noncr' `method'
end

program get_pyfilename
    args pyfilename
    tempname rr
    _return hold `rr'
    capture findfile `pyfilename'.py
    if ( !_rc ) c_local `pyfilename' `"`r(fn)'"'
    _return restore `rr'
end


if (c(stata_version) < 10) exit

version 10

mata :

mata set matastrict   on
mata set mataoptimize on

struct tuples
{
    /* input */
    string rowvector list
    real   scalar    n
    real   scalar    min
    real   scalar    max
    string rowvector conditionals
    real   scalar    is_display
    real   scalar    is_sort
    string scalar    lmacname
    
    /* derived */
    real scalar      ntuples
    real matrix      indicators
}

void tuples_ado(
    pointer(function) scalar method,
    string            scalar st_list,
    real              scalar n,
    real              scalar min,
    real              scalar max,
    string            scalar st_conditionals,
    real              scalar is_display,
    real              scalar is_sort,
    string            scalar lmacname
    )
{
    struct tuples scalar T
    
    T.list         = tokens(st_local(st_list))
    T.n            = n
    T.min          = min
    T.max          = max
    T.conditionals = tokens(st_local(st_conditionals))
    T.is_display   = is_display
    T.is_sort      = is_sort
    T.lmacname     = lmacname
    
    T.ntuples = 0
    
    (*method)(T)
    
    if (cols(T.conditionals)) 
        select_conditionals(T)
    
    if (!T.ntuples)
        tuples_c_local(T)
    
    stata(sprintf("c_local n%ss %f", T.lmacname, T.ntuples))
}

    // ---------------------------------- produce tuples
void tuples_ncr(struct tuples scalar T)
{
    real scalar    ntuples
    real scalar    r, nmr, i
    real rowvector j
    
    if (cols(T.conditionals)) {
        ncr_conditionals(T)
        return
    }
    
    T.ntuples = ntuples = rowsum(comb(T.n, (T.max..T.min)))
    
    r = T.max
    while (r >= T.min) {
        nmr = T.n-r
        j = ((i=1)..r)
        while ( i ) {
            while (i < r) j[i+1] = j[i++] + 1
            c_local_set(T, ntuples--, T.list[j])
            i = r
            while (j[i] >= nmr + i) if (!(--i)) break
            if ( i ) j[i] = j[i] + 1
        }
        --r
    }
}

void ncr_conditionals(struct tuples scalar T)
{
    real matrix    indicators
    real scalar    r, nmr, i
    real rowvector j
    
    indicators = J(T.n, 0, .)
    
    r = T.max
    while (r >= T.min) {
        T.indicators = J(T.n, (ncol=comb(T.n, r)), 0)
        nmr = T.n-r
        j = ((i=1)::r)
        while ( i ) {
            while (i < r) j[i+1] = j[i++] + 1
            T.indicators[j, ncol--] = J(r, 1, 1)
            i = r
            while (j[i] >= nmr + i) if (!(--i)) break
            if ( i ) j[i] = j[i] + 1
        }
        --r
        select_conditionals(T)
        indicators = (T.indicators, indicators)
    }
    
    T.indicators   = indicators
    T.conditionals = J(1, 0, "")
}

        // ------------------------------ historical methods
void tuples_noncr(struct tuples scalar T)
{
    real scalar N, i, csum
    
    T.indicators = J(T.n, (N=2^T.n), .)
    
    for (i=1; i<=T.n; i++)
        T.indicators[i,] = J(1, 2^(i-1), (J(1, (N=N/2), 0), J(1, N, 1)))
    
    if ((T.min == 1) & (T.max == T.n))
        T.indicators = T.indicators[|., 2\ ., .|]
    else {
        csum = colsum(T.indicators)
        T.indicators = select(T.indicators, (csum:>=T.min):&(csum:<=T.max))
    }
    
    if ((T.n > 2) & T.is_sort) {
        T.indicators = (colsum(T.indicators)\ T.indicators)'
        _sort(T.indicators, (1..cols(T.indicators)))
        T.indicators = T.indicators[|1, 2\ ., .|]'
    }
}

void tuples_kronecker(struct tuples scalar T)
{
    real matrix base, combin
    real scalar i
    
    base = combin = I(T.n)
    
    if (T.min == 1)
        T.indicators = uniqrows(base)
    
    for (i=1; i<T.min; i++)
        T.indicators = kronecker_update_combin(combin, base)
    
    for (i=T.min+1; i<=T.max; i++)
        T.indicators = T.indicators, kronecker_update_combin(combin, base)
}

real matrix kronecker_update_combin(
    real matrix combin,
    real matrix base
    )
{
    combin = (J(1, cols(combin), 1)#base) :+ (combin#J(1, cols(base), 1))
    combin = uniqrows(select(combin, !colsum(combin:==2))')'
    return(combin)
}

void tuples_cvp(struct tuples scalar T)
{
    transmorphic scalar    info
    real         scalar    i
    real         colvector p
    
    T.indicators = J(T.n, 0, 0)
    for (i=T.min; i<=T.max; i++) {
        info = cvpermutesetup((J(i, 1, 1)\ J(T.n-i, 1, 0)))
        while ((p=cvpermute(info)) != J(0, 1, .))
            T.indicators = T.indicators, p
    }
}

void tuples_naive(struct tuples scalar T)
{
    real   scalar    N, i, len, rsum
    string rowvector b
    
    N = 2^T.n-1
    
    for (i=1; i<=N; i++) {
        b = inbase(2, i)
        if (len=T.n-strlen(b)) 
            b = ("0"*len+b)
        b = subinstr(subinstr(b, "1", " 1 "), "0", " 0 ")
        T.indicators = strtoreal(tokens(b))
        if ((T.min > 1) | (T.max < T.n)) {
            rsum = rowsum(T.indicators)
            if ((rsum < T.min) | (rsum > T.max)) 
                continue
        }
        c_local_set(T, ++T.ntuples, select(T.list, T.indicators))
    }
}

    // ---------------------------------- return locals to Stata
void tuples_c_local(struct tuples scalar T)
{
    real scalar i
    
    T.ntuples = cols(T.indicators)
    for (i=1; i<=T.ntuples; i++) 
        c_local_set(T, i, select(T.list, T.indicators[, i]'))
}

void c_local_set(
    struct tuples scalar    T, 
    real          scalar    i, 
    string        rowvector tuple
    )
{
    st_local(T.lmacname, invtokens(tuple))
    stata(sprintf("c_local %s%f : copy local %s", T.lmacname, i, T.lmacname))
    if (T.is_display) 
        printf("{res}%s%f: {txt}%s\n", T.lmacname, i, st_local(T.lmacname))
}

    // ---------------------------------- conditionals
void conditionals_to_rpn(
    string scalar st_conditionals, 
    real   scalar n
    )
{
    /*
        shunting yard alike algorithm
        
            transform infix notation to reverse polish notation
        
        input: 
            st_conditionals := <lmacname> := "conditionals"
            n               := number of items in the list
            
        output:
            void
            
            replace the (infix) contents of <lmacname> in the caller
             with a space-separated reverse polish notation 
            
        only <0-9>, <&>, <|>, <!>, <(>, <)>, and <space> are allowed
        
        <space> separates statements
        
            <space> := <) & (> where
                first <space> implies preceding <(>
                last <space> omits <& (>
        
        the order of precedence is: 
            
            <!>, <&>, <|>
        
        and <!> is right-associative
            all else are left-associative
            
        perform the following checks:
        
            <0-9> must be an integer in the interval [1; <n>]
                
                <n> := length of the list of elements
                
            <&> and <|> must not be the first element in a statement
            
            <&> and <|> must not be the last element in a statement
                
                <&&> and <||> are not allowed (this is not C++)
                
            <!> must not be the last element in a statement
            
            <(> and <)> must be balanced
            
            <space> is not allowed inside parentheses
    */
    
    transmorphic scalar    t
    string       rowvector queue 
    string       colvector stack
    string       scalar    tok, top, pre
    
    t = tokeninit("", ("&", "|", "!", "~", "(", ")", " "), J(1, 0, ""))
        tokenset(t, st_local(st_conditionals))
    
    queue = J(1, 0, "")
    stack = J(0, 1, "")
    pragma unset pre
    
    while ((tok=tokenget(t)) != "") {
        if (is_valid_number(tok, n)) {
            queue = queue, tok
        }
        else if (anyof(("&", "|"), tok)) {
            if (anyof(("", "(", " "), pre))
                err_conditionals(
                    sprintf("statement may not start with %s\n", tok)
                )
            if (anyof(("&", "|", "!"), pre))
                err_conditionals(
                    sprintf("%s%s not allowed\n", pre, tok)
                )
            while (anyof(("!", "&", tok), (top=pop(stack))))
                queue = queue, top
            stack = tok\ top\ stack
        }
        else if (anyof(("!", "~"), tok)) {
            tok = "!"
            if (!anyof(("", "&", "|", "!", "(", " "), pre))
                err_conditionals(
                    sprintf("invalid %s! -- " 
                    + "missing logical operator or space\n", pre)
                )
            stack = tok\ stack
        }
        else if (tok == "(") {
            stack = tok\ stack
        }
        else if (tok == ")") {
            if (pre == "(")
                err_conditionals(
                    sprintf("invalid statement ()\n")
                )
            while ((top=pop(stack)) != J(0, 1, "")) {
                if (top == "(") 
                    break
                queue = queue, top
            }
            if (top != "(") 
                err_unmatched("close")
            confirm_not_end_with_op(pre)
            if (!anyof(("&", "|", ")", " ", ""), tokenpeek(t)))
                err_conditionals(
                    sprintf("invalid )%s -- "
                    + "missing logical operator or space\n", tokenpeek(t))
                )
        }
        else if (tok == " ") {
            if (anyof(stack, "("))
                err_unmatched("open", 0)
            confirm_not_end_with_op(pre)
            while (tokenpeek(t) == " ") 
                (void) tokenget(t)
            queue = queue, stack'
            if (cols(queue) & (tokenpeek(t)!=""))
                stack = tok\ "&"
            else 
                stack = J(0, 1, "")
        }
        else 
            err_invalid_char(tok)
        pre = tok
    }
    
    if (anyof(stack, "("))
        err_unmatched("open")
    
    confirm_not_end_with_op(pre)
    
    queue = queue, stack'
    
    st_local(st_conditionals, stritrim(invtokens(queue)))
}

real scalar is_valid_number(
    string scalar tok,
    real   scalar n
    )
{
    /* 
        returns 
            1 if tok is a valid numerical list element reference
            0 if tok is not a number (incl. missings)
        
        exits with error if tok is an invalid numerical list element reference
    */
    real scalar number
    
    pragma unset number
    
    if (_strtoreal(tok, number)) 
        return(0)
        
    if (number != trunc(number))
        err_invalid_char(".")
    
    if (number < 0)
        err_invalid_char("-")
    
    if ((number < 1) | (number > n)) {
        err_conditionals()
        errprintf("%s illegal list element reference\n", tok)
        errprintf("positional arguments must be between 1 and %f\n", n)
        exit(missing(number) ? 127 : 125)
    }
    
    return(1)
}

transmorphic matrix pop(transmorphic matrix x)
{
    /* strips and returns the first row from x */
    transmorphic rowvector top
    
    if (!rows(x)) return(J(0, cols(x), missingof(x)))
    
    top = x[1,]
    x = (rows(x)>1) ? x[|2, .\., .|] : J(0, cols(x), missingof(x))
    return(top)
}

void confirm_not_end_with_op(string scalar op)
{
    if (!anyof(("&", "|", "!"), op))
        return
    err_conditionals(
        sprintf("statement may not end with %s\n", op)
    )
}

void err_invalid_char(string scalar tok)
{
    err_conditionals()
    errprintf("%s not allowed\n", `u'substr(tok, 1, 1))
    errprintf("only digits (0-9), &, |, !, (, ), and spaces allowed\n")
    exit(198)
}

void err_unmatched(string scalar what, | real scalar unmatched)
{
    err_conditionals()
    errprintf("unmatched %s parenthesis\n", what)
    if (!unmatched)
        errprintf("statements in parentheses may not contain spaces\n")
    exit(132)
}

void err_conditionals(| string scalar errmsg)
{
    errprintf("option conditionals() invalid\n%s", errmsg)
    if (args()) exit(198)
}

void select_conditionals(struct tuples scalar T)
{
    /*
        evaluate reverse polish notation
        
        our 'stack' is not composed of scalars, e.g.,
            0, 1, ... and &, |, ...
            
        but the 'stack' is a matrix of stacked rowvectors
         and we use Mata's colon operators, e.g., :&, :| ...
    */
    
    real scalar row, i
    real matrix stack
    
    pragma unset row
    
    stack = J(0, cols(T.indicators), .)
    
    for (i=1; i<=cols(T.conditionals); i++) {
        if (!_strtoreal(T.conditionals[i], row))
            stack = T.indicators[row,]\ stack
        else if (T.conditionals[i] == "&")
            stack = (pop(stack) :& pop(stack))\ stack
        else if (T.conditionals[i] == "|") 
            stack = (pop(stack) :| pop(stack))\ stack
        else if (T.conditionals[i] == "!")
            stack = (!pop(stack))\ stack
    }
    
    T.indicators = select(T.indicators, stack)
}

end
**************************

*Main procedure

capture program drop phacker
program phacker, rclass
	version 14
	
	syntax varlist(min=3 fv ts) [if] [in],			    ///
			Method(string)						///
		    SIGma(real)	                    ///
			[                                  ///
			    KEEPvar(varlist fv ts)              ///
				FEffect(varlist fv ts)                  ///
			    MAX(numlist max=1 integer>0)                      ///
			    MIN(numlist max=1 integer>0)                      ///
				OBServation(numlist max=1 integer>0)              ///
                Head(numlist max=1 integer>0)                   ///
				SAVE                                ///
				Others(string)					///
				]
	
	marksample touse
    quietly count if `touse'
    if `r(N)' == 0 {
        error 2000
    }
	
	gettoken y cvar : varlist
	tokenize "`cvar'"
    local Qcvar = wordcount("`cvar'")
	local Qkeepvar = wordcount("`keepvar'")
	local Lenth = 2^(`Qcvar')
	
	if `sigma' <= 0 |`sigma' > 0.5 {
        local sigma 0.05
		dis _newline "     显著性选择范围限定为(0,0.5]，错误设置将自动更改为 0.05 "
    }

	if `Lenth' <= 10000 {
                local Lenth=`Lenth'+10000
            }
	 
	if "`max'" == "" {
                local max `Qcvar'
            } 
	 
	if "`min'" == "" {
                local min=1
            }
			
    if "`observation'" == "" {
                local observation=`Lenth'
            }  
			
	if "`head'" == "" {
                local head=10
            }
			
			
	dis _newline(2)  "     ************************  设置详情  ************************"
	
	if "`keepvar'" == "" {
                dis _newline  /// 
				"     * 设置待筛选变量 `Qcvar' 个, 未设置保留变量"
            }  
	else{
                dis _newline /// 
	       "     * 设置待筛选变量 `Qcvar' 个 ; 设置保留变量 `Qkeepvar' 个"
	        }
   
   if "`feffect'" == "" {
                dis _newline "     * 未设置固定效应"
            } 
	 else {
		    dis _newline "     * 已设置固定效应"
		}
	 
		    local sigper=100-(`sigma'*100)
		    dis _newline "     * 设置显著性水平为 `sigper' %"
	  
	  
	  
	 local zz=`min'
	 local gg=2^(`Qcvar')
	 local number1=`gg'-1
	 
	 while `zz' > 1 {
	 		 local zz=`zz'-1
	         local number1 = `number1'-comb(`Qcvar',`zz')
	 }
	 
	 local z=`max'
	 while `z'<`Qcvar' {
	 		 local z=`z'+1
	         local number1 = `number1'-comb(`Qcvar',`z')
	 }
	 
     dis _newline "     * 预计将生成 `number1' 组待筛选变量组合"
    
	 
	 timer clear 1
	 timer on 1
		
        qui set obs `observation'
		dis _newline(2) "                    生成中,请耐心等待！ "
		 
		 if `Qcvar' <= 19 {
                 dis _newline  "                (生成时间预计不超过 12 秒) "
            } 
	 else {
	        local minutes = int((`number1'/26000)/60) + 1
		    dis _newline  "                (生成时间预计不超过 `minutes' 分钟) "
		}
		 
	   qui macro drop number1 minutes z zz
		 
		 
		 
		
		qui tuples `cvar', max(`max') min(`min')
		
	local h 0
    forvalues u = 1/`ntuples' {
	                     local ++h
                                                       }   
	dis _newline(2) "     * 已生成 `h' 组待筛选变量组合, 即将进行回归。"
	local minutess=ceil((`h'/11)/60)
	dis _newline ///
	"     *******************  回归预计用时: `minutess'分钟  *******************"
	if "`head'" == "all" {
                local head=`h'
            }
		
		 qui `method' `y' `keepvar' `tuple1' `feffect', `others'
		 qui  gen R2 = .
         qui  gen pva = .
         qui  gen Set = ""
	 
		forvalues i = 1/`ntuples' {
			 qui{
			 	`method' `y' `keepvar' `tuple`i'' `feffect', `others'
            replace R2 = e(r2) in `i'  
		    local convar = _b[_cons]/_se[_cons]
		    gen ppvalue0=2*ttail(e(df_r),abs(`convar'))
		  local k 0
                foreach j in `tuple`i''{
	                     local ++k
						 local t = _b[`j']/_se[`j']
	                     gen ppvalue`k'=2*ttail(e(df_r),abs(`t'))
                                                       }   
			if 	"`keepvar'" != "" {
				local f 0
				foreach h in `keepvar'{
	                     local ++f
						 local t = _b[`h']/_se[`h']
	                     gen ppvalue1000`f'=2*ttail(e(df_r),abs(`t'))
			}			   	
			}
		qui{ 
          egen maxpvalue = rowmax(ppvalue*)
              gen ind = 1 if maxpvalue <= `sigma' 
          replace ind = 0 if maxpvalue > `sigma' 
		  replace pva = ind in `i'
          replace Set = "`tuple`i''" in `i'
		      drop maxpvalue ind ppvalue* 
		     macro drop k t f convar h
		     }
		  }
		}
		qui{ 
		  gen p = wordcount(Set) if Set != ""
		  gen pvalue ="√" if pva==1
		  replace pvalue="X" if pva==0
		}
		  gsort -pvalue -R2 -p           
          list R2 pvalue Set in 1/`head' ,divider sepby(pvalue)
        
		if "`save'" == "save" {
		    preserve
		    keep R2 pvalue Set
		    save "phacker_all.dta",replace
			restore  
		}
		
		qui macro drop _all   
		qui drop R2 Set p pva pvalue 
		timer off 1
		qui timer list 1
		
		dis _newline "     全体回归所用精确时间为" r(t1) "秒。"
		
		dis _newline(1) "     请查收你的结果，祝您计量愉快(^u^)"

end


