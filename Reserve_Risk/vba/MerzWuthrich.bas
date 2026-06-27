Attribute VB_Name = "MerzWuthrich"
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
' Input: a SQUARE range of INCREMENTAL claims (just the value block, no labels),
'        with the unobserved lower-right left blank. Set isCumulative:=True if
'        your block holds cumulative figures instead.
'
' Cell functions (UDFs):
'     =MW_EmergenceFactor(range, [sigmaMethod], [isCumulative])
'     =MW_OneYearSE(range, [sigmaMethod], [isCumulative])
'     =MW_UltimateSE(range, [sigmaMethod], [isCumulative])
'     =MW_TotalReserve(range, [isCumulative])
'   sigmaMethod = "loglinear" (default, matches R ChainLadder) or "mack".
'
' Macros:
'     MW_Report       - writes per-AY table + totals + emergence pattern
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
    patternSE() As Double      ' total one-year CDR SE by future calendar year
    ok As Boolean
    msg As String
End Type

'------------------------------------------------------------------------------
' Core computation. exI/exJ exclude one age-to-age ratio (0 = none); used by the
' sensitivity macro. Returns all headline figures + per-AY and pattern arrays.
'------------------------------------------------------------------------------
Private Function MW_Compute(rng As Range, ByVal sigmaMethod As String, _
                            ByVal exI As Long, ByVal exJ As Long, _
                            ByVal isCumulative As Boolean) As MWResult
    Dim res As MWResult
    Dim nR As Long, nC As Long, n As Long, i As Long, j As Long, k As Long, s As Long, c2 As Long, L As Long, cs As Long
    Dim raw As Variant

    nR = rng.Rows.Count: nC = rng.Columns.Count
    If nR <> nC Or nR < 2 Then
        res.ok = False: res.msg = "Triangle range must be square and at least 2x2.": MW_Compute = res: Exit Function
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
                    Obs(i, j) = True: Inc(i, j) = CDbl(raw(i, j))
                End If
            End If
        Next j
    Next i

    ' --- cumulative triangle (over the observed contiguous prefix of each row) ---
    Dim Cum() As Double: ReDim Cum(1 To n, 1 To n)
    Dim CObs() As Boolean: ReDim CObs(1 To n, 1 To n)
    Dim running As Double, prev As Double
    For i = 1 To n
        running = 0#
        For j = 1 To n
            If Not Obs(i, j) Then Exit For
            If isCumulative Then
                Cum(i, j) = Inc(i, j)            ' already cumulative
            Else
                running = running + Inc(i, j): Cum(i, j) = running
            End If
            CObs(i, j) = True
        Next j
    Next i

    ' --- development factors f(j), variance sigma2(j), denominator S(j) ---
    Dim f() As Double: ReDim f(1 To n - 1)
    Dim sig2() As Double: ReDim sig2(1 To n - 1)
    Dim hasSig() As Boolean: ReDim hasSig(1 To n - 1)
    Dim S() As Double: ReDim S(1 To n - 1)
    Dim num As Double, den As Double, cnt As Long, ss As Double
    For j = 1 To n - 1
        num = 0#: den = 0#: cnt = 0
        For i = 1 To n
            If CObs(i, j) And CObs(i, j + 1) And Not (i = exI And j = exJ) Then
                num = num + Cum(i, j + 1): den = den + Cum(i, j): cnt = cnt + 1
            End If
        Next i
        If den > 0 Then f(j) = num / den Else f(j) = 1#
        S(j) = den
        If cnt >= 2 Then
            ss = 0#
            For i = 1 To n
                If CObs(i, j) And CObs(i, j + 1) And Not (i = exI And j = exJ) Then
                    ss = ss + Cum(i, j) * (Cum(i, j + 1) / Cum(i, j) - f(j)) ^ 2
                End If
            Next i
            sig2(j) = ss / (cnt - 1): hasSig(j) = True
        End If
    Next j

    ' --- extrapolate missing sigma2 (typically the last column) ---
    Dim estCount As Long: estCount = 0
    For j = 1 To n - 1
        If hasSig(j) Then estCount = estCount + 1
    Next j
    Dim useLoglin As Boolean: useLoglin = (LCase(sigmaMethod) <> "mack") And (estCount >= 2)
    If useLoglin Then
        Dim Nn As Long, sx As Double, sy As Double, sxx As Double, sxy As Double, xx As Double, yy As Double, b As Double, a As Double
        Nn = 0: sx = 0: sy = 0: sxx = 0: sxy = 0
        For j = 1 To n - 1
            If hasSig(j) Then
                xx = j: yy = Log(Sqr(sig2(j)))
                Nn = Nn + 1: sx = sx + xx: sy = sy + yy: sxx = sxx + xx * xx: sxy = sxy + xx * yy
            End If
        Next j
        b = (Nn * sxy - sx * sy) / (Nn * sxx - sx * sx)
        a = (sy - b * sx) / Nn
        For j = 1 To n - 1
            If Not hasSig(j) Then sig2(j) = Exp(a + b * j) ^ 2: hasSig(j) = True
        Next j
    Else
        For j = 1 To n - 1            ' Mack min-rule fallback
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
            If j <= lastObs Then Full(i, j) = Cum(i, j) Else Full(i, j) = Full(i, j - 1) * f(j - 1)
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
            e = y * ratio(cs) / S(cs)
            For c2 = cs + 1 To n - 1
                y2 = 1#
                For k = c2 - s + 1 To c2
                    y2 = y2 * (1 - alpha(k))
                Next k
                y2 = y2 * alpha(c2 - s)
                e = e + y2 * ratio(c2) / S(c2)
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
                mn = i: If i1 < i Then mn = i1
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
            res.AY_oneYearSE(i) = 0#: res.AY_ultSE(i) = 0#
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
Public Function MW_EmergenceFactor(rng As Range, Optional sigmaMethod As String = "loglinear", _
                                   Optional isCumulative As Boolean = False) As Variant
    Dim r As MWResult: r = MW_Compute(rng, sigmaMethod, 0, 0, isCumulative)
    If Not r.ok Then MW_EmergenceFactor = r.msg: Exit Function
    MW_EmergenceFactor = r.oneYearSE / r.ultimateSE
End Function

Public Function MW_OneYearSE(rng As Range, Optional sigmaMethod As String = "loglinear", _
                             Optional isCumulative As Boolean = False) As Variant
    Dim r As MWResult: r = MW_Compute(rng, sigmaMethod, 0, 0, isCumulative)
    If Not r.ok Then MW_OneYearSE = r.msg: Exit Function
    MW_OneYearSE = r.oneYearSE
End Function

Public Function MW_UltimateSE(rng As Range, Optional sigmaMethod As String = "loglinear", _
                              Optional isCumulative As Boolean = False) As Variant
    Dim r As MWResult: r = MW_Compute(rng, sigmaMethod, 0, 0, isCumulative)
    If Not r.ok Then MW_UltimateSE = r.msg: Exit Function
    MW_UltimateSE = r.ultimateSE
End Function

Public Function MW_TotalReserve(rng As Range, Optional isCumulative As Boolean = False) As Variant
    Dim r As MWResult: r = MW_Compute(rng, "loglinear", 0, 0, isCumulative)
    If Not r.ok Then MW_TotalReserve = r.msg: Exit Function
    MW_TotalReserve = r.totalReserve
End Function

'==============================================================================
' Macro: full report (per-AY table + totals + emergence pattern)
'==============================================================================
Public Sub MW_Report()
    Dim rng As Range, isCum As Boolean, r As MWResult, ws As Worksheet, i As Long, s As Long, rowOut As Long
    On Error Resume Next
    Set rng = Application.InputBox("Select the SQUARE incremental triangle value block (no labels):", "MW Report", Type:=8)
    On Error GoTo 0
    If rng Is Nothing Then Exit Sub
    isCum = (MsgBox("Is the selected block CUMULATIVE? (No = incremental)", vbYesNo + vbQuestion, "Orientation") = vbYes)

    r = MW_Compute(rng, "loglinear", 0, 0, isCum)
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
    ws.Cells(rowOut, 1).Value = "Emergence pattern": ws.Cells(rowOut, 1).Font.Bold = True
    rowOut = rowOut + 1
    ws.Cells(rowOut, 1).Value = "Future year": ws.Cells(rowOut, 2).Value = "Total CDR SE"
    ws.Cells(rowOut, 3).Value = "Ratio to ultimate": ws.Cells(rowOut, 1).Resize(1, 3).Font.Bold = True
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
    Dim rng As Range, isCum As Boolean, base As MWResult, alt As MWResult
    Dim n As Long, i As Long, j As Long, raw As Variant, ws As Worksheet, rowOut As Long
    On Error Resume Next
    Set rng = Application.InputBox("Select the SQUARE incremental triangle value block (no labels):", "MW Sensitivity", Type:=8)
    On Error GoTo 0
    If rng Is Nothing Then Exit Sub
    isCum = (MsgBox("Is the selected block CUMULATIVE? (No = incremental)", vbYesNo + vbQuestion, "Orientation") = vbYes)

    base = MW_Compute(rng, "loglinear", 0, 0, isCum)
    If Not base.ok Then MsgBox base.msg, vbExclamation: Exit Sub
    n = base.n
    raw = rng.Value

    ' count ratios per column so we never exclude the only ratio in a column
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
                alt = MW_Compute(rng, "loglinear", i, j, isCum)
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
    ' sort by absolute impact on one-year SE (descending) via a helper column
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
    Dim g As Variant, ws As Worksheet, i As Long, j As Long, r As MWResult
    g = Array( _
        Array(357848, 766940, 610542, 482940, 527326, 574398, 146342, 139950, 227229, 67948), _
        Array(352118, 884021, 933894, 1183289, 445745, 320996, 527804, 266172, 425046, 0), _
        Array(290507, 1001799, 926219, 1016654, 750816, 146923, 495992, 280405, 0, 0), _
        Array(310608, 1108250, 776189, 1562400, 272482, 352053, 206286, 0, 0, 0), _
        Array(443160, 693190, 991983, 769488, 504851, 470639, 0, 0, 0, 0), _
        Array(396132, 937085, 847498, 805037, 705960, 0, 0, 0, 0, 0), _
        Array(440832, 847631, 1131398, 1063269, 0, 0, 0, 0, 0, 0), _
        Array(359480, 1061648, 1443370, 0, 0, 0, 0, 0, 0, 0), _
        Array(376686, 986608, 0, 0, 0, 0, 0, 0, 0, 0), _
        Array(344014, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    Set ws = MW_FreshSheet("MW_SelfTest")
    For i = 1 To 10
        For j = 1 To 10
            If g(i - 1)(j - 1) <> 0 Or (i + j - 1) <= 10 Then
                If (i + j - 1) <= 10 Then ws.Cells(i, j).Value = g(i - 1)(j - 1)
            End If
        Next j
    Next i
    r = MW_Compute(ws.Range(ws.Cells(1, 1), ws.Cells(10, 10)), "loglinear", 0, 0, False)
    Dim okOY As Boolean, okUL As Boolean
    okOY = Abs(r.oneYearSE - 1774013.8) < 1#
    okUL = Abs(r.ultimateSE - 2441364.1) < 1#
    MsgBox "GenIns self-test" & vbCrLf & _
           "One-year SE : " & Format(r.oneYearSE, "#,##0.0") & "  (ref 1,774,013.8)" & vbCrLf & _
           "Ultimate SE : " & Format(r.ultimateSE, "#,##0.0") & "  (ref 2,441,364.1)" & vbCrLf & _
           "Emergence   : " & Format(r.oneYearSE / r.ultimateSE, "0.0%") & vbCrLf & vbCrLf & _
           IIf(okOY And okUL, "PASS - matches reference.", "FAIL - check the port."), _
           IIf(okOY And okUL, vbInformation, vbExclamation), "MW_SelfTest"
End Sub

Private Function MW_FreshSheet(nm As String) As Worksheet
    Dim ws As Worksheet
    On Error Resume Next
    Set ws = ThisWorkbook.Worksheets(nm)
    Application.DisplayAlerts = False
    If Not ws Is Nothing Then ws.Delete
    Application.DisplayAlerts = True
    On Error GoTo 0
    Set ws = ThisWorkbook.Worksheets.Add
    ws.Name = nm
    Set MW_FreshSheet = ws
End Function
