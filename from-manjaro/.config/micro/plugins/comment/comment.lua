VERSION = "1.0.6"

ft = {}

ft["c"] = "// %s"
ft["go"] = "// %s"
ft["python"] = "# %s"
ft["python3"] = "# %s"
ft["html"] = "<!-- %s -->"
ft["java"] = "// %s"
ft["perl"] = "# %s"
ft["rust"] = "// %s"
ft["shell"] = "# %s"
ft["lua"] = "-- %s"
ft["javascript"] = "// %s"
ft["ruby"] = "# %s"
ft["d"] = "// %s"
ft["swift"] = "// %s"

function onViewOpen(v)
    if v.Buf.Settings["commenttype"] == nil then
        if ft[v.Buf.Settings["filetype"]] ~= nil then
            v.Buf.Settings["commenttype"] = ft[v.Buf.Settings["filetype"]]
        else
            v.Buf.Settings["commenttype"] = "# %s"
        end
    end
end

function commentLine(lineN)
    local v = CurView()
    local line = v.Buf:Line(lineN)
    local commentType = v.Buf.Settings["commenttype"]
    local commentRegex = "^%s*" .. commentType:gsub("%*", "%*"):gsub("%-", "%-"):gsub("%.", "%."):gsub("%+", "%+"):gsub("%]", "%]"):gsub("%[", "%["):gsub("%%s", "(.*)")
    local sel = -v.Buf.Cursor.CurSelection
    local curpos = -v.Buf.Cursor.Loc
    local index = string.find(commentType, "%%s") - 1
    if string.match(line, commentRegex) then
        uncommentedLine = string.match(line, commentRegex)
        v.Buf:Replace(Loc(0, lineN), Loc(#line, lineN), GetLeadingWhitespace(line) .. uncommentedLine)
        if v.Buf.Cursor:HasSelection() then
            v.Buf.Cursor.CurSelection[1].Y = sel[1].Y
            v.Buf.Cursor.CurSelection[2].Y = sel[2].Y
            v.Buf.Cursor.CurSelection[1].X = sel[1].X
            v.Buf.Cursor.CurSelection[2].X = sel[2].X
        else
            v.Buf.Cursor.X = curpos.X - index
            v.Buf.Cursor.Y = curpos.Y
        end
    else
        local commentedLine = commentType:gsub("%%s", trim(line))
        v.Buf:Replace(Loc(0, lineN), Loc(#line, lineN), GetLeadingWhitespace(line) .. commentedLine)
        if v.Buf.Cursor:HasSelection() then
            v.Buf.Cursor.CurSelection[1].Y = sel[1].Y
            v.Buf.Cursor.CurSelection[2].Y = sel[2].Y
            v.Buf.Cursor.CurSelection[1].X = sel[1].X
            v.Buf.Cursor.CurSelection[2].X = sel[2].X
        else
            v.Buf.Cursor.X = curpos.X + index
            v.Buf.Cursor.Y = curpos.Y
        end
    end
    v.Cursor:Relocate()
    v.Cursor.LastVisualX = v.Cursor:GetVisualX()
end

function commentSelection(startLine, endLine)
    for line = startLine, endLine do
        commentLine(line)
    end
end

function comment()
    local v = CurView()
    if v.Cursor:HasSelection() then
        if v.Cursor.CurSelection[1]:GreaterThan(-v.Cursor.CurSelection[2]) then
            local endLine = v.Cursor.CurSelection[1].Y
            if v.Cursor.CurSelection[1].X == 0 then
                endLine = endLine - 1
            end
            commentSelection(v.Cursor.CurSelection[2].Y, endLine)
        else
            local endLine = v.Cursor.CurSelection[2].Y
            if v.Cursor.CurSelection[2].X == 0 then
                endLine = endLine - 1
            end
            commentSelection(v.Cursor.CurSelection[1].Y, endLine)
        end
    else
        commentLine(v.Cursor.Y)
    end
end

function trim(s)
    return (s:gsub("^%s*(.-)%s*$", "%1"))
end

function string.starts(String,Start)
    return string.sub(String,1,string.len(Start))==Start
end

MakeCommand("comment", "comment.comment")
BindKey("Alt-/", "comment.comment")

AddRuntimeFile("comment", "help", "help/comment-plugin.md")
