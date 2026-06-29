' Attribute VB_Name = "MerzWuthrich"
'==============================================================================
' Merz-Wuthrich (2008) one-year Claims Development Result - ANALYTIC, in VBA
'
' Closed-form one-year reserve risk under the distribution-free chain ladder
' (Mack) model, the companion to Mack (1993) for the ultimate view. No Monte
' Carlo, no matrix algebra - pure arithmetic, so it runs natively in Excel.
'
' Ported from the Python merz_wuthrich.py and validated to reproduce R
' ChainLadder::CDR(MackChainLadder(GenIns)) exactly:
'     one-year SE 1,774,013.8 | ultimate SE 2,441,364.1 | emergence 72.7%
' Run MW_SelfTest to confirm on this machine.
'
' This module uses NO line-continuations, so it is safe to paste as plain text.
'
' Input: a SQUARE range of INCREMENTAL claims (just the value block, no labels),
'        with the unobserved lower-right left blank. Set isCumulative = True if
'        your block holds cumulative figures instead.
'
' WHICH MACRO TO RUN to analyse the triangles in your workbook:
'     MW_RunFromSetup - reads a sheet named "setup" listing each triangle and
'                       runs them all, writing "RiskEmergence_Summary".
'   (For a single triangle, select its value block and run MW_Report.)
'
' Cell functions (UDFs):
'     =MW_EmergenceFactor(range, [sigmaMethod], [isCumulative], [excludeFirstDev])
'     =MW_OneYearSE(range, [sigmaMethod], [isCumulative], [excludeFirstDev])
'     =MW_UltimateSE(range, [sigmaMethod], [isCumulative], [excludeFirstDev])
'     =MW_TotalReserve(range, [isCumulative], [excludeFirstDev])
'   sigmaMethod = "loglinear" (default, matches R ChainLadder) or "mack".
'   excludeFirstDev = TRUE drops the first development column and the immature
'     most-recent accident year (useful for long-tail lines like GL).
'
' Macros:
'     MW_RunFromSetup - batch over the "setup" sheet (main entry point)
'     MW_Portfolio    - aggregate the summary into one portfolio factor (asks rho)
'     MW_Report       - per-AY table + totals + emergence pattern for one triangle
'     MW_Sensitivity  - leave-one-out: which cells/outliers move one-year risk
'     MW_SelfTest     - validates against the GenIns reference numbers
'==============================================================================
Option Explicit
Option Base 1

Public Type MWResult
    n As Long
    oneYearSE As Double
    ultimateSE As Double
    totalReserve As Double
    AY_IBNR() As Double
    AY_oneYearSE() As Double
    AY_ultSE() As Double
    patternSE() As Double
    ok As Boolean
    msg As String
End Type

'------------------------------------------------------------------------------
' Core computation. exI/exJ exclude one age-to-age ratio (0 = none); used by the
' sensitivity macro. Returns all headline figures + per-AY and pattern arrays.
'------------------------------------------------------------------------------
Private Function MW_Compute(rng As Range, ByVal sigmaMethod As String, ByVal exI As Long, ByVal exJ As Long, ByVal isCumulative As Boolean, ByVal excludeFirstDev As Boolean) As MWResult
    Dim res As MWResult
    Dim nR As Long, nC As Long, n As Long
    Dim i As Long, j As Long, k As Long, s As Long, c2 As Long, L As Long, cs As Long
    Dim raw As Variant

    nR = rng.Rows.Count
    nC = rng.Columns.Count
    If nR <> nC Or nR < 2 Then
        res.ok = False
        res.msg = "Triangle range must be square and at least 2x2."
        MW_Compute = res
        Exit Function
    End If
    n = nR
    raw = rng.Value

    ' --- observed flags and incremental values ---
    Dim Obs() As Boolean: ReDim Obs(1 To n, 1 To n)
    Dim Inc() As Double: ReDim Inc(1 To n, 1 To n)
    For i = 1 To n
        For j = 1 To n
            If Not IsEmpty(raw(i, j)) Then
                If IsNumeric(raw(i, j)) Then
                    Obs(i, j) = True
                    Inc(i, j) = CDbl(raw(i, j))
                End If
            End If
        Next j
    Next i

    ' --- optionally drop the first development column (and the immature most-recent
    '     accident year): merge DP1 dollars into DP2, then shift left ---
    If excludeFirstDev Then
        If n < 3 Then
            res.ok = False
            res.msg = "excludeFirstDev needs at least a 3x3 triangle."
            MW_Compute = res
            Exit Function
        End If
        Dim nRed As Long: nRed = n - 1
        Dim rInc() As Double, rObs() As Boolean, jp As Long
        ReDim rInc(1 To nRed, 1 To nRed)
        ReDim rObs(1 To nRed, 1 To nRed)
        For i = 1 To nRed
            For jp = 1 To nRed
                If jp = 1 And Not isCumulative Then
                    If Obs(i, 1) And Obs(i, 2) Then
                        rInc(i, 1) = Inc(i, 1) + Inc(i, 2)
                        rObs(i, 1) = True
                    ElseIf Obs(i, 2) Then
                        rInc(i, 1) = Inc(i, 2)
                        rObs(i, 1) = True
                    End If
                Else
                    If Obs(i, jp + 1) Then
                        rInc(i, jp) = Inc(i, jp + 1)
                        rObs(i, jp) = True
                    End If
                End If
            Next jp
        Next i
        n = nRed
        Inc = rInc
        Obs = rObs
    End If

    ' --- cumulative triangle (over the observed contiguous prefix of each row) ---
    Dim Cum() As Double: ReDim Cum(1 To n, 1 To n)
    Dim CObs() As Boolean: ReDim CObs(1 To n, 1 To n)
    Dim running As Double
    For i = 1 To n
        running = 0#
        For j = 1 To n
            If Not Obs(i, j) Then Exit For
            If isCumulative Then
                Cum(i, j) = Inc(i, j)
            Else
                running = running + Inc(i, j)
                Cum(i, j) = running
            End If
            CObs(i, j) = True
        Next j
    Next i

    ' --- development factors f(j), variance sig2(j), denominator Svol(j) ---
    Dim f() As Double: ReDim f(1 To n - 1)
    Dim sig2() As Double: ReDim sig2(1 To n - 1)
    Dim hasSig() As Boolean: ReDim hasSig(1 To n - 1)
    Dim Svol() As Double: ReDim Svol(1 To n - 1)
    Dim num As Double, den As Double, cnt As Long, ssq As Double
    For j = 1 To n - 1
        num = 0#: den = 0#: cnt = 0
        For i = 1 To n
            If CObs(i, j) And CObs(i, j + 1) And Not (i = exI And j = exJ) Then
                num = num + Cum(i, j + 1)
                den = den + Cum(i, j)
                cnt = cnt + 1
            End If
        Next i
        If den > 0 Then f(j) = num / den Else f(j) = 1#
        Svol(j) = den
        If cnt >= 2 Then
            ssq = 0#
            For i = 1 To n
                If CObs(i, j) And CObs(i, j + 1) And Not (i = exI And j = exJ) Then
                    ssq = ssq + Cum(i, j) * (Cum(i, j + 1) / Cum(i, j) - f(j)) ^ 2
                End If
            Next i
            sig2(j) = ssq / (cnt - 1)
            hasSig(j) = True
        End If
    Next j

    ' --- extrapolate any missing sig2 (typically the last column) ---
    Dim estCount As Long: estCount = 0
    For j = 1 To n - 1
        If hasSig(j) Then estCount = estCount + 1
    Next j
    Dim useLoglin As Boolean
    useLoglin = (LCase(sigmaMethod) <> "mack") And (estCount >= 2)
    If useLoglin Then
        Dim npts As Long, sx As Double, sy As Double, sxx As Double, sxy As Double
        Dim xx As Double, yy As Double, slope As Double, intc As Double
        npts = 0: sx = 0: sy = 0: sxx = 0: sxy = 0
        For j = 1 To n - 1
            If hasSig(j) Then
                xx = j
                yy = Log(Sqr(sig2(j)))
                npts = npts + 1
                sx = sx + xx
                sy = sy + yy
                sxx = sxx + xx * xx
                sxy = sxy + xx * yy
            End If
        Next j
        slope = (npts * sxy - sx * sy) / (npts * sxx - sx * sx)
        intc = (sy - slope * sx) / npts
        For j = 1 To n - 1
            If Not hasSig(j) Then
                sig2(j) = Exp(intc + slope * j) ^ 2
                hasSig(j) = True
            End If
        Next j
    Else
        For j = 1 To n - 1
            If Not hasSig(j) Then
                If j >= 3 Then
                    sig2(j) = WorksheetMin(sig2(j - 1) ^ 2 / sig2(j - 2), sig2(j - 1), sig2(j - 2))
                ElseIf j >= 2 Then
                    sig2(j) = sig2(j - 1)
                Else
                    sig2(j) = 0#
                End If
                hasSig(j) = True
            End If
        Next j
    End If

    Dim ratio() As Double: ReDim ratio(1 To n - 1)
    For j = 1 To n - 1
        ratio(j) = sig2(j) / f(j) ^ 2
    Next j

    ' --- full (completed) triangle and ultimates ---
    Dim Full() As Double: ReDim Full(1 To n, 1 To n)
    Dim lastObs As Long
    For i = 1 To n
        lastObs = 0
        For j = 1 To n
            If CObs(i, j) Then lastObs = j
        Next j
        For j = 1 To n
            If j <= lastObs Then
                Full(i, j) = Cum(i, j)
            Else
                Full(i, j) = Full(i, j - 1) * f(j - 1)
            End If
        Next j
    Next i
    Dim ult() As Double: ReDim ult(1 To n)
    For i = 1 To n
        ult(i) = Full(i, n)
    Next i

    ' --- newest-diagonal weights alpha(j) (uses all observed; not excluded) ---
    Dim colsum() As Double: ReDim colsum(1 To n - 1)
    Dim alpha() As Double: ReDim alpha(1 To n - 1)
    For j = 1 To n - 1
        den = 0#
        For i = 1 To n
            If CObs(i, j) Then den = den + Cum(i, j)
        Next i
        colsum(j) = den
        If den > 0 Then alpha(j) = Cum(n - j + 1, j) / den Else alpha(j) = 0#
    Next j

    ' --- Merz-Wuthrich MSEPs (port of CL_MSEPs) ---
    Dim res3() As Double, res5() As Double, res2() As Double
    ReDim res3(1 To n, 0 To n - 2)
    ReDim res5(1 To n, 0 To n - 2)
    ReDim res2(1 To n, 0 To n - 2)
    Dim y As Double, y2 As Double, e As Double
    For i = 2 To n
        L = n - i + 1
        For s = 0 To i - 2
            cs = L + s
            res3(i, s) = ult(i) ^ 2 * ratio(cs) / Full(i, cs)
            y = 1#
            For k = L + 1 To L + s
                y = y * (1 - alpha(k))
            Next k
            e = y * ratio(cs) / Svol(cs)
            For c2 = cs + 1 To n - 1
                y2 = 1#
                For k = c2 - s + 1 To c2
                    y2 = y2 * (1 - alpha(k))
                Next k
                y2 = y2 * alpha(c2 - s)
                e = e + y2 * ratio(c2) / Svol(c2)
            Next c2
            res5(i, s) = e
            res2(i, s) = res3(i, s) + e * ult(i) ^ 2
        Next s
    Next i

    ' --- totals (process + estimation incl. cross-accident-year covariance) ---
    Dim totMSEP() As Double: ReDim totMSEP(0 To n - 2)
    Dim proc As Double, estt As Double, i1 As Long, mn As Long
    For s = 0 To n - 2
        proc = 0#
        For i = 2 To n
            If s <= i - 2 Then proc = proc + res3(i, s)
        Next i
        estt = 0#
        For i = 2 To n
            For i1 = 2 To n
                mn = i
                If i1 < i Then mn = i1
                If s <= mn - 2 Then estt = estt + res5(mn, s) * ult(i) * ult(i1)
            Next i1
        Next i
        totMSEP(s) = proc + estt
    Next s

    ' --- assemble result ---
    ReDim res.AY_IBNR(1 To n)
    ReDim res.AY_oneYearSE(1 To n)
    ReDim res.AY_ultSE(1 To n)
    ReDim res.patternSE(0 To n - 2)
    Dim msum As Double
    For i = 1 To n
        L = n - i + 1
        res.AY_IBNR(i) = ult(i) - Cum(i, L)
        If i >= 2 Then
            res.AY_oneYearSE(i) = Sqr(res2(i, 0))
            msum = 0#
            For s = 0 To i - 2
                msum = msum + res2(i, s)
            Next s
            res.AY_ultSE(i) = Sqr(msum)
        Else
            res.AY_oneYearSE(i) = 0#
            res.AY_ultSE(i) = 0#
        End If
    Next i
    Dim ultMSEP As Double: ultMSEP = 0#
    For s = 0 To n - 2
        res.patternSE(s) = Sqr(totMSEP(s))
        ultMSEP = ultMSEP + totMSEP(s)
    Next s
    res.n = n
    res.oneYearSE = Sqr(totMSEP(0))
    res.ultimateSE = Sqr(ultMSEP)
    res.totalReserve = 0#
    For i = 1 To n
        res.totalReserve = res.totalReserve + res.AY_IBNR(i)
    Next i
    res.ok = True
    MW_Compute = res
End Function

Private Function WorksheetMin(a As Double, b As Double, c As Double) As Double
    Dim m As Double: m = a
    If b < m Then m = b
    If c < m Then m = c
    WorksheetMin = m
End Function

'==============================================================================
' Cell UDFs
'==============================================================================
Public Function MW_EmergenceFactor(rng As Range, Optional sigmaMethod As String = "loglinear", Optional isCumulative As Boolean = False, Optional excludeFirstDev As Boolean = False) As Variant
    Dim r As MWResult
    r = MW_Compute(rng, sigmaMethod, 0, 0, isCumulative, excludeFirstDev)
    If Not r.ok Then MW_EmergenceFactor = r.msg: Exit Function
    MW_EmergenceFactor = r.oneYearSE / r.ultimateSE
End Function

Public Function MW_OneYearSE(rng As Range, Optional sigmaMethod As String = "loglinear", Optional isCumulative As Boolean = False, Optional excludeFirstDev As Boolean = False) As Variant
    Dim r As MWResult
    r = MW_Compute(rng, sigmaMethod, 0, 0, isCumulative, excludeFirstDev)
    If Not r.ok Then MW_OneYearSE = r.msg: Exit Function
    MW_OneYearSE = r.oneYearSE
End Function

Public Function MW_UltimateSE(rng As Range, Optional sigmaMethod As String = "loglinear", Optional isCumulative As Boolean = False, Optional excludeFirstDev As Boolean = False) As Variant
    Dim r As MWResult
    r = MW_Compute(rng, sigmaMethod, 0, 0, isCumulative, excludeFirstDev)
    If Not r.ok Then MW_UltimateSE = r.msg: Exit Function
    MW_UltimateSE = r.ultimateSE
End Function

Public Function MW_TotalReserve(rng As Range, Optional isCumulative As Boolean = False, Optional excludeFirstDev As Boolean = False) As Variant
    Dim r As MWResult
    r = MW_Compute(rng, "loglinear", 0, 0, isCumulative, excludeFirstDev)
    If Not r.ok Then MW_TotalReserve = r.msg: Exit Function
    MW_TotalReserve = r.totalReserve
End Function

'==============================================================================
' Macro: batch run from a "setup" sheet  (MAIN ENTRY POINT)
'
' The "setup" sheet lists triangles to analyse (headers in row 1, data from
' row 2):
'     A: Worksheet       - name of the sheet holding the triangle  (required)
'     B: Range           - value block, e.g. "C3:L12" (blank = sheet's UsedRange)
'     C: Cumulative      - "Y"/"N" (default N = incremental)
'     D: SigmaMethod     - "loglinear" (default) or "mack"
'     E: ExcludeFirstDev - "Y"/"N" (default N)
' Writes one row per triangle to "RiskEmergence_Summary": headline figures plus
' the emergence-by-year pattern. Bad rows are flagged, not fatal.
'==============================================================================
Public Sub MW_RunFromSetup()
    Dim setupWs As Worksheet, tgt As Worksheet, ws As Worksheet
    Dim lastRow As Long, r As Long, k As Long, total As Long, maxSteps As Long
    Dim wsName As String, rngStr As String, cumStr As String, sigM As String, exclStr As String
    Dim isCum As Boolean, exclFirst As Boolean, blk As Range, res As MWResult

    On Error Resume Next
    Set setupWs = ThisWorkbook.Worksheets("setup")
    On Error GoTo 0
    If setupWs Is Nothing Then
        MsgBox "No worksheet named 'setup' was found.", vbExclamation, "MW_RunFromSetup"
        Exit Sub
    End If

    lastRow = setupWs.Cells(setupWs.Rows.Count, 1).End(xlUp).Row
    If lastRow < 2 Then
        MsgBox "List the worksheet names in column A of 'setup' (from row 2 down).", vbExclamation, "MW_RunFromSetup"
        Exit Sub
    End If
    total = lastRow - 1

    Dim names() As String, ns() As Long, reserves() As Double
    Dim ultSE() As Double, oySE() As Double, ef() As Double, errs() As String, patterns() As Variant
    ReDim names(1 To total): ReDim ns(1 To total): ReDim reserves(1 To total)
    ReDim ultSE(1 To total): ReDim oySE(1 To total): ReDim ef(1 To total)
    ReDim errs(1 To total): ReDim patterns(1 To total)

    k = 0: maxSteps = 0
    For r = 2 To lastRow
        wsName = Trim(CStr(setupWs.Cells(r, 1).Value))
        If wsName <> "" Then
            k = k + 1
            names(k) = wsName
            rngStr = Trim(CStr(setupWs.Cells(r, 2).Value))
            cumStr = UCase(Trim(CStr(setupWs.Cells(r, 3).Value)))
            sigM = LCase(Trim(CStr(setupWs.Cells(r, 4).Value)))
            If sigM = "" Then sigM = "loglinear"
            exclStr = UCase(Trim(CStr(setupWs.Cells(r, 5).Value)))
            isCum = (cumStr = "Y" Or cumStr = "YES" Or cumStr = "TRUE" Or cumStr = "1")
            exclFirst = (exclStr = "Y" Or exclStr = "YES" Or exclStr = "TRUE" Or exclStr = "1")

            Set tgt = Nothing
            On Error Resume Next
            Set tgt = ThisWorkbook.Worksheets(wsName)
            On Error GoTo 0
            If tgt Is Nothing Then
                errs(k) = "worksheet not found"
            Else
                Set blk = Nothing
                On Error Resume Next
                If rngStr = "" Then Set blk = tgt.UsedRange Else Set blk = tgt.Range(rngStr)
                On Error GoTo 0
                If blk Is Nothing Then
                    errs(k) = "invalid range '" & rngStr & "'"
                Else
                    res = MW_Compute(blk, sigM, 0, 0, isCum, exclFirst)
                    If Not res.ok Then
                        errs(k) = res.msg
                    Else
                        ns(k) = res.n
                        reserves(k) = res.totalReserve
                        ultSE(k) = res.ultimateSE
                        oySE(k) = res.oneYearSE
                        ef(k) = res.oneYearSE / res.ultimateSE
                        patterns(k) = res.patternSE
                        If res.n - 1 > maxSteps Then maxSteps = res.n - 1
                    End If
                End If
            End If
        End If
    Next r

    If k = 0 Then
        MsgBox "No worksheet names found in 'setup'.", vbExclamation
        Exit Sub
    End If

    Set ws = MW_FreshSheet("RiskEmergence_Summary")
    ws.Range("A1").Value = "Risk emergence (analytic Merz-Wuthrich) - batch from 'setup'"
    ws.Range("A1").Font.Bold = True
    ws.Range("A2").Value = "Aggregate = One-year SE / Ultimate SE (col F). Per-development-period emergence = EF Yr columns (each = that future period's CDR SD / Ultimate SD; squares sum to 100%)."
    ws.Cells(3, 1).Value = "Worksheet": ws.Cells(3, 2).Value = "Size n"
    ws.Cells(3, 3).Value = "Total reserve": ws.Cells(3, 4).Value = "Ultimate SE"
    ws.Cells(3, 5).Value = "One-year SE": ws.Cells(3, 6).Value = "Emergence factor"
    ws.Cells(3, 7).Value = "Status"
    Dim baseCol As Long: baseCol = 8
    Dim sCol As Long
    For sCol = 1 To maxSteps
        ws.Cells(3, baseCol + sCol - 1).Value = "EF Yr" & sCol
    Next sCol
    ws.Range(ws.Cells(3, 1), ws.Cells(3, baseCol + maxSteps - 1)).Font.Bold = True

    Dim i As Long, outRow As Long, p As Variant, sIdx As Long
    outRow = 4
    For i = 1 To k
        ws.Cells(outRow, 1).Value = names(i)
        If errs(i) <> "" Then
            ws.Cells(outRow, 7).Value = "ERROR: " & errs(i)
        Else
            ws.Cells(outRow, 2).Value = ns(i)
            ws.Cells(outRow, 3).Value = reserves(i)
            ws.Cells(outRow, 4).Value = ultSE(i)
            ws.Cells(outRow, 5).Value = oySE(i)
            ws.Cells(outRow, 6).Value = ef(i): ws.Cells(outRow, 6).NumberFormat = "0.0%"
            ws.Cells(outRow, 7).Value = "OK"
            p = patterns(i)
            For sIdx = 0 To ns(i) - 2
                ws.Cells(outRow, baseCol + sIdx).Value = p(sIdx) / ultSE(i)
                ws.Cells(outRow, baseCol + sIdx).NumberFormat = "0.0%"
            Next sIdx
        End If
        outRow = outRow + 1
    Next i
    ws.Columns.AutoFit
    ws.Activate
    MsgBox "Processed " & k & " worksheet(s) listed in 'setup'.", vbInformation, "MW_RunFromSetup"
End Sub

'==============================================================================
' Macro: portfolio aggregation across LoBs (single correlation rho)
'
' Reads per-LoB one-year SE and ultimate SE from "RiskEmergence_Summary" and
' aggregates each with a single pairwise rho, then divides. Emergence factors are
' ratios of SDs, so the dollar SEs are aggregated WITH correlation - you cannot
' average the factors directly. rho largely cancels in the ratio, so the factor
' is robust; the independence / full-correlation bookends are shown too.
'==============================================================================
Public Sub MW_Portfolio()
    Dim ws As Worksheet, hdr As Long, c As Long
    Dim colWs As Long, colOY As Long, colUL As Long, colStat As Long

    On Error Resume Next
    Set ws = ThisWorkbook.Worksheets("RiskEmergence_Summary")
    On Error GoTo 0
    If ws Is Nothing Then
        MsgBox "Run MW_RunFromSetup first (no 'RiskEmergence_Summary' sheet).", vbExclamation
        Exit Sub
    End If

    hdr = 3
    For c = 1 To 30
        Select Case CStr(ws.Cells(hdr, c).Value)
            Case "Worksheet": colWs = c
            Case "One-year SE": colOY = c
            Case "Ultimate SE": colUL = c
            Case "Status": colStat = c
        End Select
    Next c
    If colOY = 0 Or colUL = 0 Then
        MsgBox "Could not find the 'One-year SE' / 'Ultimate SE' columns.", vbExclamation
        Exit Sub
    End If

    Dim rhoStr As String, rho As Double
    rhoStr = InputBox("Correlation rho between LoBs (0 = independent, 1 = fully correlated):", "Portfolio aggregation", "0.25")
    If rhoStr = "" Then Exit Sub
    If Not IsNumeric(rhoStr) Then MsgBox "rho must be numeric.", vbExclamation: Exit Sub
    rho = CDbl(rhoStr)
    If rho < 0 Or rho > 1 Then MsgBox "rho must be between 0 and 1.", vbExclamation: Exit Sub

    Dim r As Long, lastRow As Long, cnt As Long
    Dim sumO As Double, sumU As Double, sumOO As Double, sumUU As Double, oVal As Double, uVal As Double
    r = hdr + 1
    Do While Trim(CStr(ws.Cells(r, colWs).Value)) <> ""
        If colStat = 0 Or UCase(Trim(CStr(ws.Cells(r, colStat).Value))) = "OK" Then
            If IsNumeric(ws.Cells(r, colOY).Value) And IsNumeric(ws.Cells(r, colUL).Value) Then
                oVal = CDbl(ws.Cells(r, colOY).Value)
                uVal = CDbl(ws.Cells(r, colUL).Value)
                sumO = sumO + oVal
                sumU = sumU + uVal
                sumOO = sumOO + oVal * oVal
                sumUU = sumUU + uVal * uVal
                cnt = cnt + 1
            End If
        End If
        lastRow = r
        r = r + 1
    Loop
    If cnt = 0 Then MsgBox "No OK rows with numeric SEs found.", vbExclamation: Exit Sub

    Dim portO As Double, portU As Double
    portO = Sqr(rho * sumO * sumO + (1 - rho) * sumOO)
    portU = Sqr(rho * sumU * sumU + (1 - rho) * sumUU)

    Dim w As Long: w = lastRow + 2
    ws.Cells(w, 1).Value = "Portfolio (rho = " & Format(rho, "0.00") & ", " & cnt & " LoBs)"
    ws.Cells(w, 1).Font.Bold = True
    ws.Cells(w + 1, 1).Value = "Portfolio one-year SE": ws.Cells(w + 1, 2).Value = portO
    ws.Cells(w + 2, 1).Value = "Portfolio ultimate SE": ws.Cells(w + 2, 2).Value = portU
    ws.Cells(w + 3, 1).Value = "Portfolio emergence factor": ws.Cells(w + 3, 2).Value = portO / portU
    ws.Cells(w + 3, 2).NumberFormat = "0.0%"
    ws.Cells(w + 4, 1).Value = "  bookend: independent (rho=0)"
    ws.Cells(w + 4, 2).Value = Sqr(sumOO) / Sqr(sumUU): ws.Cells(w + 4, 2).NumberFormat = "0.0%"
    ws.Cells(w + 5, 1).Value = "  bookend: fully correlated (rho=1)"
    ws.Cells(w + 5, 2).Value = sumO / sumU: ws.Cells(w + 5, 2).NumberFormat = "0.0%"
    ws.Cells(w + 6, 1).Value = "Diversification benefit (one-year)"
    ws.Cells(w + 6, 2).Value = 1 - portO / sumO: ws.Cells(w + 6, 2).NumberFormat = "0.0%"
    ws.Columns("A:B").AutoFit
    ws.Activate
    MsgBox "Portfolio emergence factor = " & Format(portO / portU, "0.0%") & "  (rho " & Format(rho, "0.00") & ", " & cnt & " LoBs)", vbInformation, "MW_Portfolio"
End Sub

'==============================================================================
' Macro: full report (per-AY table + totals + emergence pattern) for one triangle
'==============================================================================
Public Sub MW_Report()
    Dim rng As Range, isCum As Boolean, exclFirst As Boolean, r As MWResult
    Dim ws As Worksheet, i As Long, s As Long, rowOut As Long
    On Error Resume Next
    Set rng = Application.InputBox("Select the SQUARE incremental triangle value block (no labels):", "MW Report", Type:=8)
    On Error GoTo 0
    If rng Is Nothing Then Exit Sub
    isCum = (MsgBox("Is the selected block CUMULATIVE? (No = incremental)", vbYesNo + vbQuestion, "Orientation") = vbYes)
    exclFirst = (MsgBox("Exclude the first development column (and the immature most-recent accident year)?", vbYesNo + vbQuestion, "Exclude first dev") = vbYes)

    r = MW_Compute(rng, "loglinear", 0, 0, isCum, exclFirst)
    If Not r.ok Then MsgBox r.msg, vbExclamation: Exit Sub

    Set ws = MW_FreshSheet("MW_Results")
    ws.Range("A1").Value = "Merz-Wuthrich one-year reserve risk"
    ws.Range("A1").Font.Bold = True
    ws.Range("A3").Value = "Emergence factor (one-year / ultimate)"
    ws.Range("B3").Value = r.oneYearSE / r.ultimateSE: ws.Range("B3").NumberFormat = "0.0%"
    ws.Range("A4").Value = "One-year SE": ws.Range("B4").Value = r.oneYearSE
    ws.Range("A5").Value = "Ultimate SE": ws.Range("B5").Value = r.ultimateSE
    ws.Range("A6").Value = "Total reserve": ws.Range("B6").Value = r.totalReserve

    ws.Range("A8").Value = "AY": ws.Range("B8").Value = "IBNR"
    ws.Range("C8").Value = "One-year CDR SE": ws.Range("D8").Value = "Ultimate Mack SE"
    ws.Range("A8:D8").Font.Bold = True
    rowOut = 9
    For i = 1 To r.n
        ws.Cells(rowOut, 1).Value = i
        ws.Cells(rowOut, 2).Value = r.AY_IBNR(i)
        ws.Cells(rowOut, 3).Value = r.AY_oneYearSE(i)
        ws.Cells(rowOut, 4).Value = r.AY_ultSE(i)
        rowOut = rowOut + 1
    Next i
    ws.Cells(rowOut, 1).Value = "Total"
    ws.Cells(rowOut, 2).Value = r.totalReserve
    ws.Cells(rowOut, 3).Value = r.oneYearSE
    ws.Cells(rowOut, 4).Value = r.ultimateSE
    ws.Cells(rowOut, 1).Resize(1, 4).Font.Bold = True

    rowOut = rowOut + 2
    ws.Cells(rowOut, 1).Value = "Risk emergence by future development period": ws.Cells(rowOut, 1).Font.Bold = True
    rowOut = rowOut + 1
    ws.Cells(rowOut, 1).Value = "Future period": ws.Cells(rowOut, 2).Value = "Total CDR SE"
    ws.Cells(rowOut, 3).Value = "Emergence (CDR SD / ult SD)": ws.Cells(rowOut, 1).Resize(1, 3).Font.Bold = True
    For s = 0 To r.n - 2
        rowOut = rowOut + 1
        ws.Cells(rowOut, 1).Value = s + 1
        ws.Cells(rowOut, 2).Value = r.patternSE(s)
        ws.Cells(rowOut, 3).Value = r.patternSE(s) / r.ultimateSE
        ws.Cells(rowOut, 3).NumberFormat = "0.0%"
    Next s
    ws.Columns("A:D").AutoFit
    ws.Activate
End Sub

'==============================================================================
' Macro: leave-one-out outlier sensitivity (impact on the one-year SE)
'==============================================================================
Public Sub MW_Sensitivity()
    Dim rng As Range, isCum As Boolean, exclFirst As Boolean, base As MWResult, alt As MWResult
    Dim n As Long, i As Long, j As Long, ws As Worksheet, rowOut As Long
    On Error Resume Next
    Set rng = Application.InputBox("Select the SQUARE incremental triangle value block (no labels):", "MW Sensitivity", Type:=8)
    On Error GoTo 0
    If rng Is Nothing Then Exit Sub
    isCum = (MsgBox("Is the selected block CUMULATIVE? (No = incremental)", vbYesNo + vbQuestion, "Orientation") = vbYes)
    exclFirst = (MsgBox("Exclude the first development column (and the immature most-recent accident year)?", vbYesNo + vbQuestion, "Exclude first dev") = vbYes)

    base = MW_Compute(rng, "loglinear", 0, 0, isCum, exclFirst)
    If Not base.ok Then MsgBox base.msg, vbExclamation: Exit Sub
    n = base.n

    Dim colCnt() As Long: ReDim colCnt(1 To n - 1)
    For j = 1 To n - 1
        For i = 1 To n - j
            colCnt(j) = colCnt(j) + 1
        Next i
    Next j

    Set ws = MW_FreshSheet("MW_Sensitivity")
    ws.Range("A1").Value = "Leave-one-out sensitivity (impact of excluding each age-to-age ratio)"
    ws.Range("A1").Font.Bold = True
    ws.Range("A3:F3").Value = Array("AY", "Dev period", "d_oneYear_SE", "d_ultimate_SE", "d_reserve", "d_emergence")
    ws.Range("A3:F3").Font.Bold = True
    rowOut = 4
    Dim baseEF As Double: baseEF = base.oneYearSE / base.ultimateSE
    For i = 1 To n
        For j = 1 To n - 1
            If i <= n - j And colCnt(j) > 1 Then
                alt = MW_Compute(rng, "loglinear", i, j, isCum, exclFirst)
                If alt.ok Then
                    ws.Cells(rowOut, 1).Value = i
                    ws.Cells(rowOut, 2).Value = j
                    ws.Cells(rowOut, 3).Value = alt.oneYearSE - base.oneYearSE
                    ws.Cells(rowOut, 4).Value = alt.ultimateSE - base.ultimateSE
                    ws.Cells(rowOut, 5).Value = alt.totalReserve - base.totalReserve
                    ws.Cells(rowOut, 6).Value = (alt.oneYearSE / alt.ultimateSE) - baseEF
                    rowOut = rowOut + 1
                End If
            End If
        Next j
    Next i
    If rowOut > 4 Then
        ws.Range("H4:H" & rowOut - 1).Formula = "=ABS(C4)"
        ws.Range("A3:H" & rowOut - 1).Sort Key1:=ws.Range("H3"), Order1:=xlDescending, Header:=xlYes
        ws.Columns("H").Delete
    End If
    ws.Columns("A:F").AutoFit
    ws.Activate
End Sub

'==============================================================================
' Self-test: build the GenIns triangle in code and check the answer
'==============================================================================
Public Sub MW_SelfTest()
    Dim g(1 To 10) As Variant, ws As Worksheet, i As Long, j As Long, r As MWResult, verdict As String
    g(1) = Array(357848, 766940, 610542, 482940, 527326, 574398, 146342, 139950, 227229, 67948)
    g(2) = Array(352118, 884021, 933894, 1183289, 445745, 320996, 527804, 266172, 425046)
    g(3) = Array(290507, 1001799, 926219, 1016654, 750816, 146923, 495992, 280405)
    g(4) = Array(310608, 1108250, 776189, 1562400, 272482, 352053, 206286)
    g(5) = Array(443160, 693190, 991983, 769488, 504851, 470639)
    g(6) = Array(396132, 937085, 847498, 805037, 705960)
    g(7) = Array(440832, 847631, 1131398, 1063269)
    g(8) = Array(359480, 1061648, 1443370)
    g(9) = Array(376686, 986608)
    g(10) = Array(344014)
    Set ws = MW_FreshSheet("MW_SelfTest")
    For i = 1 To 10
        For j = 1 To 11 - i
            ws.Cells(i, j).Value = g(i)(j - 1)
        Next j
    Next i
    r = MW_Compute(ws.Range(ws.Cells(1, 1), ws.Cells(10, 10)), "loglinear", 0, 0, False, False)
    Dim okOY As Boolean, okUL As Boolean
    okOY = Abs(r.oneYearSE - 1774013.8) < 1#
    okUL = Abs(r.ultimateSE - 2441364.1) < 1#
    If okOY And okUL Then verdict = "PASS - matches reference." Else verdict = "FAIL - check the port."
    MsgBox "GenIns self-test" & vbCrLf & "One-year SE : " & Format(r.oneYearSE, "#,##0.0") & "  (ref 1,774,013.8)" & vbCrLf & "Ultimate SE : " & Format(r.ultimateSE, "#,##0.0") & "  (ref 2,441,364.1)" & vbCrLf & "Emergence   : " & Format(r.oneYearSE / r.ultimateSE, "0.0%") & vbCrLf & vbCrLf & verdict, IIf(okOY And okUL, vbInformation, vbExclamation), "MW_SelfTest"
End Sub

Private Function MW_FreshSheet(nm As String) As Worksheet
    ' Reuse the sheet if it already exists (clearing old contents), else create it.
    ' Reusing preserves a pre-built / pre-positioned sheet rather than deleting it.
    Dim ws As Worksheet
    On Error Resume Next
    Set ws = ThisWorkbook.Worksheets(nm)
    On Error GoTo 0
    If ws Is Nothing Then
        Set ws = ThisWorkbook.Worksheets.Add
        ws.Name = nm
    Else
        ws.Cells.Clear
    End If
    Set MW_FreshSheet = ws
End Function
