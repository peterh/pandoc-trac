-- A sample custom reader for Trac (wiki markup)
-- https://trac.edgewall.org/wiki/WikiFormatting

-- For better performance we put these functions in local variables:
local P, S, R, Cf, Cc, Ct, V, Cs, Cg, Cb, B, C, Cmt =
  lpeg.P, lpeg.S, lpeg.R, lpeg.Cf, lpeg.Cc, lpeg.Ct, lpeg.V,
  lpeg.Cs, lpeg.Cg, lpeg.Cb, lpeg.B, lpeg.C, lpeg.Cmt

local whitespacechar = S(" \t\r\n")
local specialchar = S("/*~[]\\{}|!'_")
local wordchar = (1 - (whitespacechar + specialchar))
local spacechar = S(" \t")
local newline = P"\r"^-1 * P"\n"
local blankline = spacechar^0 * newline
local endline = newline * #-blankline
local endequals = spacechar^0 * P"="^0 * spacechar^0 * newline
local cellsep = spacechar^0 * P"||"
local tablestart = newline * cellsep
local upper = R"AZ"
local lower = R"az"
local alpha = R("AZ", "az")
local digit = R"09"
local listtok = P"*" + digit^1 * P"."
local liststart = newline * spacechar^0 * listtok

local function trim(s)
   return (s:gsub("^%s*(.-)%s*$", "%1"))
end

local function ListItem(lev, start)
  local subitem = function(c)
    if lev < 6 then
      return ListItem(lev + 1, c)
    else
      return (1 - 1) -- fails
    end
  end
  local parser = spacechar^(lev*2+1)
               * start
               * spacechar^1
               * Ct((V"Inline" - liststart - newline)^0)
               * newline
               * (Ct(subitem(P"*")^1) / pandoc.BulletList
                  +
                  Ct(subitem(digit^1 * P".")^1) / pandoc.OrderedList
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
  Para = Ct((V"Inline" - liststart - tablestart - newline)^1)
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
  BulletList = Ct(ListItem(0, P"*")^1)
             / pandoc.BulletList ;
  OrderedList = Ct(ListItem(0, digit^1 * P".")^1)
             / pandoc.OrderedList ;
  Table = Ct(V"TableRow"^1)
        / function(bodyrows)
            local numcolumns = #(bodyrows[1])
            local headerrow = table.remove(bodyrows, 1)
            local aligns = {}
            local widths = {}
            for i = 1,numcolumns do
              aligns[i] = pandoc.AlignDefault
              widths[i] = 0
            end
            return pandoc.utils.from_simple_table(
              pandoc.SimpleTable({}, aligns, widths, headerrow, bodyrows))
          end ;
  TableRow   = Ct(V"BodyCell"^1)
             * cellsep
             * spacechar^0
             * newline ;
  BodyCell   = cellsep
             * spacechar^0
             * Ct((V"Inline" - (newline + cellsep))^0)
             * #cellsep
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
                fn = string.match(txt, "Image%((%g+)%)")
                if fn then
                  local url = "attachments/wiki/" .. PANDOC_STATE.input_files[1] .. "/" .. fn
                  return pandoc.Image(pandoc.Str(url), url)
                end
                return pandoc.Str("")
              end ;
  Code = P"{{{"
       * C((1 - P"}}}")^0)
       * P"}}}"
       / trim / pandoc.Code ;
  Link = C(upper^1 * lower^1 * upper^1 * alpha^1)
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
      * C((P"http" * P"s"^-1 + P"mailto")
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
         * Ct((V"Inline" -P"'")^1)
         * P"'''"
         / pandoc.Strong ;
  Emph2 = P"''"
       * Ct((V"Inline" - (P"''" * -P"'"))^1)
       * P"''"
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
