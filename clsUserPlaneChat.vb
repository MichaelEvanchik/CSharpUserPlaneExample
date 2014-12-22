Imports SeasideResearch.LibCurlNet
Imports System.Text
Imports System.Text.RegularExpressions
Imports System.Net.Sockets


Public Class clsUserPlaneChat
    Inherits clsRTMPSock
    Implements IDisposable

    Public m_nPing As Integer = 0

    Private web As New clsHttpClient
    Private m_sHost As String
    Private m_nPort As Integer = 443

    Private m_sSpawnId As String
    Private m_sRspLocal As String
    Private m_sInstanceId As String
    Private m_sDomainId As String

    Private m_sChatUrl As String
    Private m_sScreenName As String
    Private m_sPassword As String

    Private m_nReConPause As Integer = 3    'seconds

    Public Event Authenticated()
    Public Event Join(ByVal screenname As String)
    Public Event Join2(ByVal screenname As String)
    Public Event Part(ByVal screenname As String)
    Public Event Part2(ByVal screenname As String)
    Public Event ChatText(ByVal screenname As String, ByVal message As String)
    Public Event IMRequest(ByVal screenname As String, ByVal id As String)

    Public Property Password() As String
        Get
            Password = m_sPassword
        End Get
        Set(ByVal pw As String)
            m_sPassword = pw
        End Set
    End Property
    Public Property ScreenName() As String
        Get
            ScreenName = m_sScreenName
        End Get
        Set(ByVal sn As String)
            m_sScreenName = sn
        End Set
    End Property
    Public Property ChatURL() As String
        Get
            ChatURL = m_sChatUrl
        End Get
        Set(ByVal url As String)
            m_sChatUrl = url
        End Set
    End Property
    Public Property DomainId() As String
        Get
            DomainId = m_sDomainId
        End Get
        Set(ByVal host As String)
            m_sDomainId = host
        End Set
    End Property
    Public Property InstanceId() As String
        Get
            InstanceId = m_sInstanceId
        End Get
        Set(ByVal id As String)
            m_sInstanceId = id
        End Set
    End Property

    Sub Dispose() Implements IDisposable.Dispose
        web.Dispose()
    End Sub

    Private Function GetSpawnID() As String

        Dim spawnid As String = vbNullString

        Dim rtmp As New clsRTMPSock

        Dim page As String = vbNullString
        Dim url As String = "http://api.userplane.com/flashservices/gateway"
        Dim post As String
        Dim size As Integer = 110 + Len(DomainId) + Len(InstanceId)
        Dim user As String = "Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.8.0.12) Gecko/20070508 Firefox/1.5.0.12"
        Dim http As String = _
            "POST /flashservices/gateway HTTP/1.1" & vbCrLf & _
            "Accept-Encoding: gzip, deflate" & vbCrLf & _
            "Host: api.userplane.com" & vbCrLf & _
            "User-Agent: " & user & vbCrLf & _
            "Accept: text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5" & vbCrLf & _
            "Accept-Language: en-us,en;q=0.5" & vbCrLf & _
            "---------------: ------------" & vbCrLf & _
            "Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7" & vbCrLf & _
            "Keep-Alive: 300" & vbCrLf & _
            "Connection: close" & vbCrLf & _
            "Content-type: application/x-amf" & vbCrLf & _
            "Content-length: " & size & vbCrLf & vbCrLf


        Dim amf2 As Integer = TypeMarker.TString
        Dim amf3 As Integer = TypeMarker.TObject

        Dim unk0() As Byte = {0, 0, 0, 0, 0, 1}
        Dim rpid() As Byte = {0, 0, 0, &H80, &HA, 0, 0, 0, 1}

        With Encoding.Default
            post = _
                .GetString(unk0) & _
                HexStr("000e") & "server.do.exec" & _
                HexStr("0002") & "/1" & _
                .GetString(rpid) & _
                MyBase.AMFEncode(amf3, _
                        MyBase.SetField(amf2, "call", "loadBalancer.getServer") & _
                        MyBase.SetField(amf2, "domainID", DomainId) & _
                        MyBase.SetField(amf2, "instanceID", InstanceId) & _
                        MyBase.SetField(amf2, "spawnID", "") _
                    ) & _
                ""

        End With

        Dim sock As New Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
        Dim recv(1023) As Byte
        Dim read As Integer

        sock.Connect("api.userplane.com", 80)

        sock.Send(Encoding.Default.GetBytes(http), Len(http), SocketFlags.None)
        sock.Send(Encoding.Default.GetBytes(post), Len(post), SocketFlags.None)

        Do
            read = sock.Receive(recv, 1024, SocketFlags.None)
            page &= Left(Encoding.Default.GetString(recv), read)
        Loop While read
        sock.Close()

        'Debug.Print(HexDump(page))
        Dim data As String

        If InStr(page, "SERVERNAME") Then
            data = Split(page, "SERVERNAME")(1)
            data = Mid(data, 4)
            data = Split(data, Chr(0))(0)
            m_sHost = data
        Else
            Debug.Print("[!] problem obtaining server name")
        End If
        If InStr(page, "SPAWNID") Then
            data = Split(page, "SPAWNID")(1)
            data = Mid(data, 2, 8)
            'Dim tmp() As Byte = System.Text.Encoding.Default.GetBytes(StrReverse(data))
            'Dim sid As Long
            'sid = BitConverter.ToUInt32(tmp, 4)
            'sid = ((sid And &H1FFFFF) * 4) + (tmp(3) \ 64)
            'spawnid = CStr(sid)
            'Debug.Print(HexDump(data))
            spawnid = SpawnHax(data)
            Debug.Print("[#] spawnid [" & Left(HexDump(data), 23) & "] => " & spawnid)
            m_sSpawnId = spawnid
        Else
            Debug.Print("[!] problem obtaining spawn id")
            GetRspLocal()
            Connect()
        End If

        Return spawnid

    End Function

    Private Function SpawnHax(ByVal hexspawn As String) As String

        Dim mk As clsRTMPSock.TypeMarker
        Dim tval As Object = Nothing

        'Debug.Print("[~] spawn hax 0_q...")
        'Debug.Print(HexDump(hexspawn))
        AMFDecode(Chr(0) & StrReverse(hexspawn), mk, tval)

        tval = tval \ &H20000000
        tval = tval And &H7FFFFF&
        tval = tval + &H800000&

        Return CStr(tval)
        'Return "8653599"

    End Function

    Public Function GetChatInfo(Optional ByVal url As String = vbNullString, Optional ByVal ref As String = vbNullString) As Boolean

        Dim page As String = vbNullString

        Dim rersplocal As New Regex("fo\.addVariable\(""strKey"",\s""([^""]*)""\);", RegexOptions.IgnoreCase)
        Dim rechatid As New Regex("fo\.addVariable\(""strInstanceID"",\s""([^""]*)""\);", RegexOptions.IgnoreCase)

        Dim rsplocal As String = vbNullString
        Dim chatid As String = vbNullString

        If Len(url) > 0 Then m_sChatUrl = url
        Try
            url = m_sChatUrl
            If Not web.Fetch(page, url, ref) Then
                web.SaveAndRun("c:\chatpage.htm", True)
                Debug.Print("[!] unable to fetch landing page")
                Return False
            End If

            rsplocal = rersplocal.Match(page).Groups(1).Value
            chatid = rechatid.Match(page).Groups(1).Value
            If Len(rsplocal) = 0 Then
                Debug.Print("[!] unable to obtain rspLocal!")
                If InStr(page, "You could not enter this chat because you have been banned for violations") Then Debug.Print("[!] user is banned from aim/userplane chats!")
                web.SaveAndRun("c:\wtf.htm", True)
                Return False
            End If
            Debug.Print("[+] rspLocal = """ & rsplocal & """")
        Catch ex As Exception
            Debug.Print("[!] getChatInfo : " & ex.ToString.ToLower)
            If Len(rsplocal) = 0 Then Debug.Print("[!] unable to determine 'rsplocal'.")
            If Len(chatid) = 0 Then Debug.Print("[!] unable to determine 'chatid'.")
            Return False
        End Try

        m_sRspLocal = rsplocal
        m_sInstanceId = chatid
        Return True

    End Function

    Public Function GetRspLocal(Optional ByVal sn As String = vbNullString, Optional ByVal pw As String = vbNullString) As Boolean

        Dim post As String
        Dim url As String
        Dim ref As String
        Dim page As String = vbNullString

        Dim remcurl As New Regex("<body\sonLoad=""checkErrorAndSubmitForm\('loginForm',\s'false',\s'([^']*)'\);"">", RegexOptions.IgnoreCase)
        Dim reusrd As New Regex("name=""usrd""\svalue=""(\d*)""", RegexOptions.IgnoreCase)

        Dim mcurl As String = vbNullString
        Dim usrd As String = vbNullString

        If Len(sn) = 0 Then sn = m_sScreenName
        If Len(pw) = 0 Then pw = m_sPassword

        Try
            web.Dispose()
            web = New clsHttpClient
            web.m_cEasy.SetOpt(CURLoption.CURLOPT_SSL_VERIFYPEER, 0)
            ref = vbNullString
            url = "https://my.screenname.aol.com/_cqr/login/login.psp?mcState=initialized&uitype=mini&sitedomain=chat.aim.com&authLev=1&seamless=y&lang=en&locale=us&siteState=OrigUrl%3Dhttp%253A%252F%252Fchat.aim.com%252F&_sns_width_=174&_sns_height_=196&_sns_fg_color_=333333&_sns_err_color_=C81A1A&_sns_link_color_=2864B4&_sns_bg_color_=e2e0e1"
            If Not web.Fetch(page, url, ref, "fetching login portal") Then
                Debug.Print("[!] unable to fetch login portal")
                Exit Function
            End If

            usrd = reusrd.Match(page).Groups(1).Value
            post = _
                "sitedomain=" & "chat.aim.com" & _
                "&siteId=" & "" & _
                "&lang=" & "en" & _
                "&locale=" & "us" & _
                "&authLev=" & "1" & _
                "&siteState=" & "OrigUrl%253Dhttp%25253A%25252F%25252Fchat.aim.com%25252F" & _
                "&isSiteStateEncoded=" & "true" & _
                "&mcState=" & "initialized" & _
                "&uitype=" & "mini" & _
                "&use_aam=" & "0" & _
                "&_sns_fg_color_=" & "333333" & _
                "&_sns_err_color_=" & "C81A1A" & _
                "&_sns_link_color_=" & "2864B4" & _
                "&_sns_width_=" & "174" & _
                "&_sns_height_=" & "196" & _
                "&_sns_bg_color_=" & "e2e0e1" & _
                "&offerId=" & "" & _
                "&seamless=" & "y" & _
                "&regPromoCode=" & "" & _
                "&idType=" & "SN" & _
                "&usrd=" & usrd & _
                "&loginId=" & sn & _
                "&password=" & pw & _
                "&rememberMe=" & "off"

            ref = url
            url = "https://my.screenname.aol.com/_cqr/login/login.psp"
            If Not web.PostData(page, url, ref, post, "posting authentication information") Then
                Debug.Print("[!] unable to post login")
                Return False
            End If
            'web.SaveAndRun("c:\logdin.htm", True)

            If InStr(page, "Invalid Screen Name or Password. Please try again.") Then
                Debug.Print("[!] this account is dead.")
                Return False
            End If

            mcurl = remcurl.Match(page).Groups(1).Value
            If Len(mcurl) = 0 Then
                Debug.Print("[!] unable to obtain magic carpet url")
                Return False
            End If
            ref = url
            url = mcurl
            If Not web.Fetch(page, url, ref) Then
                web.SaveAndRun("c:\magiccarpet.htm", True)
                Debug.Print("[!] unable to fetch mc page")
                Return False
            End If

            ref = url
            If Not GetChatInfo(m_sChatUrl) Then
                Return False
            End If

        Catch ex As Exception
            Debug.Print("[!] getRspLocal : " & ex.ToString.ToLower)
            If Len(usrd) = 0 Then Debug.Print("[!] unable to determine 'usrd'.")
            If Len(mcurl) = 0 Then Debug.Print("[!] unable to determine 'mcurl'.")
            Return False
        End Try

        Return True

    End Function

    Public Sub SendChat(ByVal msg As String)

        MyBase.SendPacket(3, clsRTMPSock.RMTPDatatype.Invoke, _
                MyBase.AMFEncode(clsRTMPSock.TypeMarker.TString, "sendChatText") & _
                MyBase.AMFEncode(clsRTMPSock.TypeMarker.TNumber, 0) & _
                MyBase.AMFEncode(clsRTMPSock.TypeMarker.TNull, 0) & _
                MyBase.AMFEncode(clsRTMPSock.TypeMarker.TString, msg) & _
                MyBase.AMFEncode(clsRTMPSock.TypeMarker.TString, "1") & _
                MyBase.AMFEncode(clsRTMPSock.TypeMarker.TUndefined, 0) _
            )

    End Sub

    Public Function OpenIM(ByVal sn As String, ByRef im As clsUserPlaneIM) As Boolean

        Dim upim As clsUserPlaneIM
        Dim url As String
        Dim page As String = vbNullString

        Dim rekey As New Regex("fo\.addVariable\(""key"",\s""([^""]*)""\);", RegexOptions.IgnoreCase)
        Dim key As String

        url = "http://chat.aim.com/embed/webMessenger?targetMemberID=" & sn & "&instanceID=" & m_sInstanceId
        If web.Fetch(page, url) Then
            key = rekey.Match(page).Groups(1).Value
            key = System.Web.HttpUtility.UrlDecode(key)

            upim = New clsUserPlaneIM(m_sDomainId, m_sInstanceId, sn, m_sScreenName, key)
            upim.Connect()
            im = upim
            Return True
        Else
            Debug.Print("[-] unable to launch im")
            Return False
        End If

    End Function

    Public Sub LaunchIC(ByVal sn As String)

        '
        MyBase.SendPacket(3, clsRTMPSock.RMTPDatatype.Invoke, _
                MyBase.AMFEncode(clsRTMPSock.TypeMarker.TString, "launchIC") & _
                MyBase.AMFEncode(clsRTMPSock.TypeMarker.TNumber, 0) & _
                MyBase.AMFEncode(clsRTMPSock.TypeMarker.TNull, 0) & _
                MyBase.AMFEncode(clsRTMPSock.TypeMarker.TString, sn) _
            )

    End Sub

    Public Sub Whisper(ByVal sn As String, ByVal msg As String)

        m_nPing += 1
        Debug.Print("[*] whispering to " & sn & ": " & msg)
        MyBase.SendPacket(3, clsRTMPSock.RMTPDatatype.Invoke, _
                MyBase.AMFEncode(clsRTMPSock.TypeMarker.TString, "whisper") & _
                MyBase.AMFEncode(clsRTMPSock.TypeMarker.TNumber, 0) & _
                MyBase.AMFEncode(clsRTMPSock.TypeMarker.TNull, 0) & _
                MyBase.AMFEncode(clsRTMPSock.TypeMarker.TString, sn) & _
                MyBase.AMFEncode(clsRTMPSock.TypeMarker.TString, msg) _
            )

    End Sub

    Public Overrides Sub Connect(Optional ByVal host As String = vbNullString, Optional ByVal port As Integer = -1)

        m_sDomainId = "chat.aim.com"
        'm_sInstanceId = "4d5f52d6-3646-102b-95db-00188b3382f7" 'love shack
        'm_sInstanceId = "9a0432e8-2ca3-102b-95db-00188b3382f7"

        m_sSpawnId = GetSpawnID()
        m_nPing = 0

        With MyBase.Connection
            .app = "CommunicationSuite/chat.aim.com" & _
                "/" & m_sHost & _
                "/" & m_sInstanceId & _
                "/" & m_sSpawnId
            '.flashVer = ""
            .swfUrl = "http://cache.static.userplane.com/CommunicationSuite/ch.swf"
            .tcUrl = "rtmp://" & m_sHost & ":" & m_nPort & "/CommunicationSuite/chat.aim.com" & _
                "/" & m_sHost & _
                "/" & m_sInstanceId & _
                "/" & m_sSpawnId
            .fpad = False
            .audioCodecs = 3703616
            .videoCodecs = 21312
            .pageUrl = m_sChatUrl
        End With

        Dim amf2 As Byte = clsRTMPSock.TypeMarker.TString
        Dim amf5 As Byte = clsRTMPSock.TypeMarker.TNull
        Dim amf6 As Byte = clsRTMPSock.TypeMarker.TUndefined

        MyBase.Host = m_sHost
        MyBase.Port = m_nPort

        Dim rsp As String = m_sRspLocal
        'rsp = System.Web.HttpUtility.UrlDecode(rsp)
        'rsp = System.Web.HttpUtility.UrlDecode(rsp)

        MyBase.ConnectData = _
            MyBase.AMFEncode(amf2, "SWF") & _
            MyBase.AMFEncode(amf2, m_sScreenName) & _
            MyBase.AMFEncode(amf2, rsp) & _
            MyBase.AMFEncode(amf2, "ch") & _
            MyBase.AMFEncode(amf5, 0) & _
            MyBase.AMFEncode(amf6, 0) & _
            MyBase.AMFEncode(amf2, "020100")

        RSleep(m_nReConPause * 1000)
        Console.WriteLine("[*] connecting to [" & MyBase.Host & ":" & MyBase.Port & "]...")
        MyBase.Connect()

    End Sub

    Private Sub clsUserPlaneChat_Disconnected() Handles Me.Disconnected

        'CloseSocket(False)
        'Debug.Print("[~] (" & m_sScreenName & ") socket closed and reconnecting")
        'RSleep(m_nReConPause * 1000)
        'Connect()

    End Sub

    Private Sub clsUserPlaneChat_Invoke(ByVal invoke As String, ByVal invokeid As Long, ByVal nullbyte As Byte, ByVal data As String) Handles Me.Invoke

        Debug.Print("[*] (" & m_sScreenName & ") INVOKE: " & invoke)
        Select Case invoke
            Case "_result"
                'instr "NetConnection.Connect.Success"  - connected!
                'Debug.Print(HexDump(data))
                Debug.Print("[*] _result")
            Case "onDisconnect"
                'instr "User.NotAuthorized" - rsp is stale
                Debug.Print("[!] onDisconnect!@?")
                CloseSocket(False)
                For timeitout As Integer = 1 To 600
                    Windows.Forms.Application.DoEvents()
                    System.Threading.Thread.Sleep(100)
                Next
                If GetRspLocal() Then
                    'System.Threading.Thread.Sleep(3000)
                    Connect()
                Else
                    Debug.Print("[!] unable to login account")
                End If
            Case "onStatus"
                Debug.Print("[*] onStatus")
            Case "onAuth"
                PublishStatus(invokeid)
                RaiseEvent Authenticated()
            Case Else
                Debug.Print("[!] unknown invoke: '" & invoke & "'... (+)")
        End Select

    End Sub

    Private Sub PublishStatus(Optional ByVal invokeid As Long = 0)

        SendPacket(3, RMTPDatatype.SharedObject, _
                SetField(TypeMarker.TNumber, "prefs/domain_so", 0) & _
                HexStr("00 00 00 01 00 00 00 00") _
            )   '*

        SendPacket(3, RMTPDatatype.SharedObject, _
                SetField(TypeMarker.TNumber, "prefs/server_so", 0) & _
                HexStr("00 00 00 01 00 00 00 00") _
            )   '*

        '=====
        'j
        For loopi As Integer = 1 To 6
            SendPacket(3, RMTPDatatype.Invoke, _
                    AMFEncode(TypeMarker.TString, "watchUser") & _
                    AMFEncode(TypeMarker.TNumber, 0) & _
                    AMFEncode(TypeMarker.TNull, 0) & _
                    AMFEncode(TypeMarker.TString, ScreenName) & _
                    AMFEncode(TypeMarker.TNumber, 0) _
                )
        Next

        'h
        SendPacket(3, RMTPDatatype.SharedObject, _
                SetField(TypeMarker.TNumber, "users/" & ScreenName & "/model_so", 0) & _
                HexStr("00 00 00 01 00 00 00 00") _
            )   '*

        SendPacket(3, RMTPDatatype.SharedObject, _
                SetField(TypeMarker.TNumber, "users/" & ScreenName & "/watchers_so", 0) & _
                HexStr("00 00 00 01 00 00 00 00") _
            )   '*

        SendPacket(3, RMTPDatatype.SharedObject, _
                SetField(TypeMarker.TNumber, "users/" & ScreenName & "/swfs_so", 0) & _
                HexStr("00 00 00 01 00 00 00 00") _
            )

        '---
        If invokeid Then
            Debug.Print("[!] onAuth, _result...")
            SendPacket(3, RMTPDatatype.Invoke, _
                    AMFEncode(TypeMarker.TString, "_result") & _
                    AMFEncode(TypeMarker.TNumber, invokeid) & _
                    AMFEncode(TypeMarker.TNull, 0) & _
                    AMFEncode(TypeMarker.TUndefined, 0) _
                )   '*
        End If
        '---

        'Exit Sub

        'f
        SendPacket(3, RMTPDatatype.SharedObject, _
                SetField(TypeMarker.TNumber, "rooms/public/1/userList_so", 0) & _
                HexStr("00 00 00 01 00 00 00 00") _
            )   '*

        SendPacket(3, RMTPDatatype.SharedObject, _
                SetField(TypeMarker.TNumber, "rooms/public/1/model_so", 0) & _
                HexStr("00 00 00 01 00 00 00 00") _
            )   '*

        SendPacket(3, RMTPDatatype.SharedObject, _
                SetField(TypeMarker.TNumber, "rooms/public/1/dispatch_so", 0) & _
                HexStr("00 00 00 01 00 00 00 00") _
            )   '*

        '---
        SendPacket(3, RMTPDatatype.Invoke, _
                AMFEncode(TypeMarker.TString, "getRoomHistory") & _
                AMFEncode(TypeMarker.TNumber, 64) & _
                AMFEncode(TypeMarker.TNull, 0) & _
                AMFEncode(TypeMarker.TString, "1") _
            )   '*
        '---

        SendPacket(3, RMTPDatatype.SharedObject, _
                SetField(TypeMarker.TNumber, "rooms/public/roomList_so", 0) & _
                HexStr("00 00 00 01 00 00 00 00") _
            )

        'SendPacket(3, RMTPDatatype.SharedObject, _
        '        SetField(TypeMarker.TNumber, "messageKeyManager/messageKeys_so", 0) & _
        '        HexStr("00 00 00 01 00 00 00 00") _
        '    )   '*

        '+++
        SendPacket(3, RMTPDatatype.SharedObject, _
                SetField(TypeMarker.TNumber, "rooms/public/1/model_so", 2883584) & _
                HexStr("00 00 00 01 00 00 00 00") _
            )   '*
        SendPacket(3, RMTPDatatype.SharedObject, _
                SetField(TypeMarker.TNumber, "rooms/public/1/model_so", 2883584) & _
                HexStr("00 00 00 01 00 00 00 00") _
            )   '*
        '738197504

    End Sub

    Private Sub PublishStatus2()

        For loopi As Integer = 1 To 6
            SendPacket(3, RMTPDatatype.Invoke, _
                    AMFEncode(TypeMarker.TString, "watchUser") & _
                    AMFEncode(TypeMarker.TNumber, 0) & _
                    AMFEncode(TypeMarker.TNull, 0) & _
                    AMFEncode(TypeMarker.TString, ScreenName) & _
                    AMFEncode(TypeMarker.TNumber, 0) _
                )
        Next

        'f
        SendPacket(3, RMTPDatatype.SharedObject, _
                SetField(TypeMarker.TNumber, "rooms/public/1/userList_so", 0) & _
                HexStr("00 00 00 01 00 00 00 00") _
            )   '*

        SendPacket(3, RMTPDatatype.SharedObject, _
                SetField(TypeMarker.TNumber, "rooms/public/1/model_so", 0) & _
                HexStr("00 00 00 01 00 00 00 00") _
            )   '*

        SendPacket(3, RMTPDatatype.SharedObject, _
                SetField(TypeMarker.TNumber, "rooms/public/1/dispatch_so", 0) & _
                HexStr("00 00 00 01 00 00 00 00") _
            )   '*

        '---
        SendPacket(3, RMTPDatatype.Invoke, _
                AMFEncode(TypeMarker.TString, "getRoomHistory") & _
                AMFEncode(TypeMarker.TNumber, 64) & _
                AMFEncode(TypeMarker.TNull, 0) & _
                AMFEncode(TypeMarker.TString, "1") _
            )   '*
        '---

        SendPacket(3, RMTPDatatype.SharedObject, _
                SetField(TypeMarker.TNumber, "rooms/public/roomList_so", 0) & _
                HexStr("00 00 00 01 00 00 00 00") _
            )

        'SendPacket(3, RMTPDatatype.SharedObject, _
        '        SetField(TypeMarker.TNumber, "messageKeyManager/messageKeys_so", 0) & _
        '        HexStr("00 00 00 01 00 00 00 00") _
        '    )   '*

    End Sub

    Private Sub clsUserPlaneChat_SharedObject(ByVal objname As String, ByVal objdata As String) Handles Me.SharedObject

        Select Case objname
            Case "rooms/public/1/dispatch_so"
                If Len(objdata) >= 17 Then
                    'Debug.Print(HexDump(objdata))
                    objdata = Mid(objdata, 18)
                    Dim mk As clsRTMPSock.TypeMarker
                    Dim devent As String = vbNullString
                    Dim sn As String = vbNullString
                    Dim text As String = vbNullString

                    MyBase.AMFDecode(objdata, mk, devent, False)
                    Select Case devent
                        Case "onJoin"
                            MyBase.AMFDecode(objdata, mk, sn, False)
                            Debug.Print("[+] (" & m_sScreenName & ") " & sn & " entered the chat.")
                            RaiseEvent Join(sn)

                        Case "onLeave"
                            MyBase.AMFDecode(objdata, mk, sn, False)
                            Debug.Print("[-] (" & m_sScreenName & ") " & sn & " exited the chat.")
                            RaiseEvent Part(sn)

                        Case "sendChatText"
                            Try
                                MyBase.AMFDecode(objdata, mk, text, False)
                                MyBase.AMFDecode(objdata, mk, sn, False)
                                RaiseEvent ChatText(sn, text)
                            Catch ex As Exception
                                Debug.Print("problem with room obj...")
                                Debug.Print(HexDump(objdata))
                            End Try
                            'Debug.Print("[.] " & sn & ": " & text)
                        Case Else
                            Debug.Print("unknown room event: " & devent)
                    End Select

                End If

            Case "rooms/public/1/userList_so"
                'Debug.Print(HexDump(objdata))
                If Len(objdata) >= 17 Then
                    Dim act As Byte = Asc(Mid(objdata, 16, 1))
                    Dim unk As Byte = Asc(Mid(objdata, 17, 1))

                    If unk > 0 Then
                        Dim sn As String = MyBase.GetField(Mid(objdata, 18))
                        If act = 1 Then RaiseEvent Join2(sn)
                        If act = 0 Then RaiseEvent Part2(sn)
                    End If
                End If

            Case "rooms/public/1/model_so"
                'Debug.Print("[.] room object: """ & objname & """")
                'Debug.Print(HexDump(objdata))

            Case "users/" & m_sScreenName & "/model_so"

            Case "users/" & m_sScreenName & "/swfs_so"
                Dim copy As String = Mid(objdata, 18)
                Dim act As String = vbNullString
                Dim sn As String = vbNullString
                Dim id As String = vbNullString
                Dim mk As TypeMarker

                'Debug.Print(HexDump(objdata))
                If Left(copy, 1) = Chr(8) Then
                    Debug.Print("[~] holy room debug event!")
                    'PublishStatus2()
                End If
                If Left(copy, 1) = Chr(2) Then
                    If Not AMFDecode(copy, mk, act, False) Then Exit Sub
                    If Not AMFDecode(copy, mk, sn, False) Then Exit Sub
                    If Not AMFDecode(copy, mk, id, False) Then Exit Sub
                    Select Case act
                        Case "launchIC"
                            Debug.Print("[+] sn: " & sn)
                            Debug.Print("[+] id: " & id)
                            RaiseEvent IMRequest(sn, id)
                        Case "whisper"
                            Debug.Print("[~] " & id & " whispers: " & sn)
                            m_nPing -= 1
                        Case Else
                            Debug.Print("[!] unknown 'swfs_so' object action: " & act)
                    End Select
                End If
                'GoTo dumpit

            Case "rooms/public/roomList_so"

            Case Else
dumpit:
                Debug.Print("[?] unknown object: """ & objname & """")
                'Debug.Print(HexDump(objdata))
                'PublishStatus()

        End Select

    End Sub

    Private Sub clsUserPlaneChat_SockError(ByVal ex As System.Net.Sockets.SocketException) Handles Me.SockError

        Debug.Print("[!] (" & m_sScreenName & ") sockerr : " & ex.ErrorCode & " : " & ex.Message.ToLower)

    End Sub
End Class
