
Public Class clsUserPlaneIM
    Inherits clsRTMPSock

    Private m_sHost As String = "chat.aim.com"
    Private m_sInstanceId As String = "9a0432e8-2ca3-102b-95db-00188b3382f7"
    Public m_sTarget As String = "mcbain"
    Public m_sScreenName As String = "mcbain"
    Private m_sKey As String = "%2FwQCAAAAAABX%2FJUTSiyU7KpVS0WBO%2FQBu%2BMuTdnjevuyGFdjq6t8ZgnE%2BP51H4hhz8kwCJE0ATrfd8sX"

    Public Event IsConnected(ByVal sender As Object, ByVal connected As Boolean)
    Public Event ChatText(ByVal sender As Object, ByVal text As String)
    Public Event ChatAction(ByVal sender As Object, ByVal action As String)
    Public Event ChatTyping(ByVal sender As Object, ByVal typing As Boolean)
    Public Event Blocked(ByVal sender As Object, ByVal bstatus As Boolean)
    Public Event SockClose(ByVal sender As Object)
    Public Event LoadBalance(ByVal sender As Object, ByVal server As String, ByVal key As String)

    Public Overrides Sub Connect(Optional ByVal host As String = vbNullString, Optional ByVal port As Integer = -1)

        With MyBase.Connection
            .app = "InstantCommunicator" & _
                "/" & m_sHost & _
                "/" & m_sInstanceId
            '.flashVer = ""
            .swfUrl = "http://cache.static.userplane.com/InstantCommunicator/ic.swf"
            .tcUrl = "rtmp://ic.flashcom.aol.userplane.com:443/InstantCommunicator" & _
                "/" & m_sHost & _
                "/" & m_sInstanceId
            .fpad = False
            .audioCodecs = 3703616
            .videoCodecs = 21312
            .pageUrl = "http://chat.aim.com/embed/webMessenger?targetMemberID=" & m_sTarget & "&instanceID=" & m_sInstanceId
        End With

        Dim amf2 As Byte = clsRTMPSock.TypeMarker.TString
        Dim amf1 As Byte = clsRTMPSock.TypeMarker.TBoolean

        MyBase.ConnectData = _
            MyBase.AMFEncode(amf2, m_sScreenName) & _
            MyBase.AMFEncode(amf2, m_sKey) & _
            MyBase.AMFEncode(amf2, m_sTarget) & _
            MyBase.AMFEncode(amf1, False) & _
            MyBase.AMFEncode(amf2, "") & _
            MyBase.AMFEncode(amf2, "ic")

        MyBase.Connect("ic.flashcom.aol.userplane.com", 443)

    End Sub

    Public Sub SetAudio(ByVal enabled As Boolean)
        SendChatCmd("audio", enabled, m_sScreenName & "_" & m_sTarget)
    End Sub
    Public Sub SetVideo(ByVal enabled As Boolean)
        SendChatCmd("video", enabled, m_sScreenName & "_" & m_sTarget)
    End Sub
    Public Sub SetTyping(ByVal typing As Boolean)
        SendChatCmd("chat", "typing", typing)
    End Sub
    Public Sub SendChat(ByVal message As String)
        SendChatEx("<FONT FACE=""Arial"" SIZE=""12"" COLOR=""#000000"" LETTERSPACING=""0"" KERNING=""0"">" & message & "</FONT><br>")
    End Sub
    Public Sub SendChatEx(ByVal message As String)
        SendChatCmd("chat", "text", message)
    End Sub
    Public Sub SetAway(ByVal away As Boolean)
        SendChatCmd("setAwayStatus", away)
    End Sub

    Private Sub SendChatCmd(ByVal arg1 As String, ByVal arg2 As Boolean, ByVal arg3 As String)
        MyBase.SendPacket(3, clsRTMPSock.RMTPDatatype.Invoke, _
                AMFEncode(TypeMarker.TString, "sendChatCommand") & _
                AMFEncode(TypeMarker.TNumber, 0) & _
                AMFEncode(TypeMarker.TNull, 0) & _
                AMFEncode(TypeMarker.TString, "") & _
                AMFEncode(TypeMarker.TString, arg1) & _
                AMFEncode(TypeMarker.TBoolean, arg2) & _
                AMFEncode(TypeMarker.TString, arg3) _
            )
    End Sub
    Private Sub SendChatCmd(ByVal arg1 As String, ByVal arg2 As String, ByVal arg3 As Boolean)
        MyBase.SendPacket(3, clsRTMPSock.RMTPDatatype.Invoke, _
                AMFEncode(TypeMarker.TString, "sendChatCommand") & _
                AMFEncode(TypeMarker.TNumber, 0) & _
                AMFEncode(TypeMarker.TNull, 0) & _
                AMFEncode(TypeMarker.TString, "") & _
                AMFEncode(TypeMarker.TString, arg1) & _
                AMFEncode(TypeMarker.TString, arg2) & _
                AMFEncode(TypeMarker.TBoolean, arg3) _
            )
    End Sub
    Private Sub SendChatCmd(ByVal arg1 As String, ByVal arg2 As String, ByVal arg3 As String)
        MyBase.SendPacket(3, clsRTMPSock.RMTPDatatype.Invoke, _
                AMFEncode(TypeMarker.TString, "sendChatCommand") & _
                AMFEncode(TypeMarker.TNumber, 0) & _
                AMFEncode(TypeMarker.TNull, 0) & _
                AMFEncode(TypeMarker.TString, "") & _
                AMFEncode(TypeMarker.TString, arg1) & _
                AMFEncode(TypeMarker.TString, arg2) & _
                AMFEncode(TypeMarker.TString, arg3) _
            )
    End Sub
    Private Sub SendChatCmd(ByVal arg1 As String, ByVal arg2 As Boolean)
        MyBase.SendPacket(3, clsRTMPSock.RMTPDatatype.Invoke, _
                AMFEncode(TypeMarker.TString, "sendChatCommand") & _
                AMFEncode(TypeMarker.TNumber, 0) & _
                AMFEncode(TypeMarker.TNull, 0) & _
                AMFEncode(TypeMarker.TString, "") & _
                AMFEncode(TypeMarker.TString, arg1) & _
                AMFEncode(TypeMarker.TBoolean, arg2) & _
                AMFEncode(TypeMarker.TNull, 0) _
            )
    End Sub

    Private Sub clsUserPlaneIM_Disconnected() Handles Me.Disconnected

        RaiseEvent SockClose(Me)

    End Sub

    Private Sub clsUserPlaneIM_Invoke(ByVal invoke As String, ByVal invokeid As Long, ByVal nullbyte As Byte, ByVal data As String) Handles Me.Invoke

        Dim mk As clsRTMPSock.TypeMarker
        Dim fval As Object = Nothing

        Debug.Print("[*] UPIM.INVOKE: " & invoke)
        Select Case invoke
            Case "_result"

            Case "destinationIsConnected"
                RaiseEvent IsConnected(Me, IIf((Mid(data, 2, 1) = Chr(1)), True, False))

            Case "sendChatCommand"
                Dim upf As String
                Dim cmd As String

                AMFDecode(data, mk, fval) : upf = fval
                AMFDecode(data, mk, fval) : cmd = fval

                Select Case upf
                    Case "chat"
                        Select Case cmd
                            Case "action"
                                AMFDecode(data, mk, fval) : RaiseEvent ChatAction(Me, fval)
                            Case "text"
                                AMFDecode(data, mk, fval) : RaiseEvent ChatText(Me, fval)
                            Case "typing"
                                AMFDecode(data, mk, fval) : RaiseEvent ChatTyping(Me, fval)
                            Case "receiveGameInvitation"
                                Debug.Print("[@] game invitation!")
                                Debug.Print(HexDump(data))
                            Case Else
                                Debug.Print("[!] unknown chat command: """ & cmd & """")
                        End Select
                    Case Else
                        Debug.Print("[!] unknown command type: """ & upf & """")
                End Select

            Case "setDomainPreferences"
                SetAudio(False)
                SetVideo(False)

            Case "setMemberPreferences"

            Case "setBlockedStatus"
                RaiseEvent Blocked(Me, IIf((Mid(data, 2, 1) = Chr(1)), True, False))

            Case "setMemberInfo"

            Case "onDisconnect"
                If InStr(data, "LoadBalance.Redirect") Then
                    Debug.Print("[@] load balancing request")
                    'If InStr(m_sServer, "fcs") Then Exit Sub
                    'Debug.Print(HexDump(data))
                End If

            Case "onStatus"

        End Select

    End Sub

    Private Sub clsUserPlaneIM_SharedObject(ByVal objname As String, ByVal objdata As String) Handles Me.SharedObject

        Select Case objname
            Case Else
                Debug.Print("UP.IM> unknown object: """ & objname & """")
                Debug.Print(HexDump(objdata))

        End Select

    End Sub

    Public Sub New(ByVal host As String, ByVal instanceId As String, ByVal target As String, ByVal sn As String, ByVal key As String)

        m_sHost = host
        m_sInstanceId = instanceId
        m_sTarget = target
        m_sScreenName = sn
        m_sKey = key

    End Sub

End Class
