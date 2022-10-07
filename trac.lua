-- A sample custom reader for Trac (wiki markup)
-- https://trac.edgewall.org/wiki/WikiFormatting

-- For better performance we put these functions in local variables:
local P, S, R, Cf, Cc, Ct, V, Cs, Cg, Cb, B, C, Cmt =
  lpeg.P, lpeg.S, lpeg.R, lpeg.Cf, lpeg.Cc, lpeg.Ct, lpeg.V,
  lpeg.Cs, lpeg.Cg, lpeg.Cb, lpeg.B, lpeg.C, lpeg.Cmt

local whitespacechar = S(" \t\r\n")
local specialchar = S("/*~[]\\{}|!'")
local wordchar = (1 - (whitespacechar + specialchar))
local spacechar = S(" \t")
local newline = P"\r"^-1 * P"\n"
local blankline = spacechar^0 * newline
local endline = newline * #-blankline
local endequals = spacechar^0 * P"="^0 * spacechar^0 * newline
local cellsep = spacechar^0 * P"|"

local function trim(s)
   return (s:gsub("^%s*(.-)%s*$", "%1"))
end

local function ListItem(lev, ch)
  local start
  if ch == nil then
    start = S"*#"
  else
    start = P(ch)
  end
  local subitem = function(c)
    if lev < 6 then
      return ListItem(lev + 1, c)
    else
      return (1 - 1) -- fails
    end
  end
  local parser = spacechar^0
               * start^lev
               * #(- start)
               * spacechar^0
               * Ct((V"Inline" - (newline * spacechar^0 * S"*#"))^0)
               * newline
               * (Ct(subitem("*")^1) / pandoc.BulletList
                  +
                  Ct(subitem("#")^1) / pandoc.OrderedList
                  +
                  Cc(nil))
               / function (ils, sublist)
                   return { pandoc.Plain(ils), sublist }
                 end
  return parser
end

-- Grammar
local G = P{ "Doc",
  Doc = Ct(V"Block"^0)
      / pandoc.Pandoc ;
  Block = blankline^0
        * ( V"Header"
          + V"HorizontalRule"
          + V"CodeBlock"
          + V"List"
          + V"Table"
          + V"Para") ;
  Para = Ct(V"Inline"^1)
       * newline
       / pandoc.Para ;
  HorizontalRule = spacechar^0
                 * P"----"
                 * spacechar^0
                 * newline
                 / pandoc.HorizontalRule;
  Header = (P("=")^1 / string.len)
         * spacechar^1
         * Ct((V"Inline" - endequals)^1)
         * endequals
         / pandoc.Header;
  CodeBlock = P"{{{"
            * blankline
            * C((1 - (newline * P"}}}"))^0)
            * newline
            * P"}}}"
            / pandoc.CodeBlock;
  Placeholder = P"<<<"
              * C(P(1) - P">>>")^0
              * P">>>"
              / function() return pandoc.Div({}) end;
  List = V"BulletList"
       + V"OrderedList" ;
  BulletList = Ct(ListItem(1,'*')^1)
             / pandoc.BulletList ;
  OrderedList = Ct(ListItem(1,'#')^1)
             / pandoc.OrderedList ;
  Table = (V"TableHeader" + Cc{})
        * Ct(V"TableRow"^1)
        / function(headrow, bodyrows)
            local numcolumns = #(bodyrows[1])
            local aligns = {}
            local widths = {}
            for i = 1,numcolumns do
              aligns[i] = pandoc.AlignDefault
              widths[i] = 0
            end
            return pandoc.utils.from_simple_table(
              pandoc.SimpleTable({}, aligns, widths, headrow, bodyrows))
          end ;
  TableHeader = Ct(V"HeaderCell"^1)
              * cellsep^-1
              * spacechar^0
              * newline ;
  TableRow   = Ct(V"BodyCell"^1)
             * cellsep^-1
             * spacechar^0
             * newline ;
  HeaderCell = cellsep
             * P"="
             * spacechar^0
             * Ct((V"Inline" - (newline + cellsep))^0)
             / function(ils) return { pandoc.Plain(ils) } end ;
  BodyCell   = cellsep
             * spacechar^0
             * Ct((V"Inline" - (newline + cellsep))^0)
             / function(ils) return { pandoc.Plain(ils) } end ;
  Inline = V"Emph"
         + V"Strong"
         + V"Strong2"
         + V"Emph2"
         + V"Strike"
         + V"Underline"
         + V"Superscript"
         + V"Subscript"
         + V"LineBreak"
         + V"Macro"
         + V"Link"
         + V"Link2"
         + V"URL"
         + V"URLLink"
         + V"Image"
         + V"Str"
         + V"Space"
         + V"SoftBreak"
         + V"Escaped"
         + V"Placeholder"
         + V"Code"
         + V"Special" ;
  Str = wordchar^1
      / pandoc.Str;
  Escaped = P"!"
          * C(P(1))
          / pandoc.Str ;
  Special = specialchar
          / pandoc.Str;
  Space = spacechar^1
        / pandoc.Space ;
  SoftBreak = endline
            * # -(V"HorizontalRule" + V"CodeBlock")
            / pandoc.SoftBreak ;
  LineBreak = P"\\\\"
            / pandoc.LineBreak ;
  Macro = P"[[" * C(wordchar^1) * P"]]"
            / function(txt)
                if string.lower(txt) == "br" then
                  return pandoc.LineBreak()
                end
                return pandoc.Str("")
              end ;
  Code = P"{{{"
       * C((1 - P"}}}")^0)
       * P"}}}"
       / trim / pandoc.Code ;
  Link = C(R"AZ"^1 * R"az"^1 * R"AZ"^1 * R("AZ", "az")^1)
       / function(txt)
           return pandoc.Link(txt, txt)
         end ;
  Link2 = P"[wiki:"
       * C(wordchar^1)
       * (spacechar * C((wordchar+spacechar)^1))^0
       * P"]"
       / function(url, desc)
           local txt = desc or {pandoc.Str(url)}
           return pandoc.Link(txt, url)
         end ;
  Image = P"{{"
        * #-P"{"
        * C((1 - (S"}"))^0)
        * (P"|" * Ct((V"Inline" - P"}}")^1))^-1
        * P"}}"
        / function(url, desc)
            local txt = desc or ""
            return pandoc.Image(txt, url)
          end ;
  URL = P"http"
      * P"s"^-1
      * P":"
      * (1 - (whitespacechar + (S",.?!:;\"'" * #whitespacechar)))^1
      / function(url)
          if string.find(url, ".gif", -4) or string.find(url, ".png", -4) or string.find(url, ".jpg", -4) then
            return pandoc.Image(pandoc.Str(url), url)
          end
          return pandoc.Link(pandoc.Str(url), url)
        end ;
  URLLink = P"["
      * C("http"
      * P"s"^-1
      * P":"
      * (1 - whitespacechar)^1)
      * whitespacechar
      * C((1 - P"]")^1)
      * P"]"
      / function(url, desc)
          return pandoc.Link(desc, url)
        end ;
  Emph = P"//"
       * Ct((V"Inline" - P"//")^1)
       * P"//"
       / pandoc.Emph ;
  Strong = P"**"
         * Ct((V"Inline" -P"**")^1)
         * P"**"
         / pandoc.Strong ;
  Strong2 = P"'''"
         * Ct((V"Inline" -P"'''")^1)
         * P"'''"
         / pandoc.Strong ;
  Emph2 = P"''"
       * #-P"'"
       * Ct((V"Inline" - P"''")^1)
       * P"''"
       * #-P"'"
       / pandoc.Emph ;
  Strike = P"~~"
         * Ct((V"Inline" -P"~~")^1)
         * P"~~"
         / pandoc.Strikeout ;
  Underline = P"__"
         * Ct((V"Inline" -P"__")^1)
         * P"__"
         / pandoc.Underline ;
  Subscript = P",,"
         * Ct((V"Inline" -P",,")^1)
         * P",,"
         / pandoc.Subscript ;
  Superscript = P"^"
         * Ct((V"Inline" -P"^")^1)
         * P"^"
         / pandoc.Superscript ;
}

function Reader(input, reader_options)
  return lpeg.match(G, tostring(input))
end
