
> module Code.LaTeX (
>                    table, 
>                    longTable,
>                    multiRow,
>                    multiColumn,
>                    macro,
>) where

> import Text.PrettyPrint


> -- | Pretty print a TeX macro.
> macro :: String -- ^ name of the macro
>       -> [Doc]  -- ^ the arguments (need not give the grouping braces)
>       -> Doc   
> macro name args = text "\\" <> text name <> argdoc
>     where argdoc = hcat $ map braces args

> -- | A horizontal line.

> hline =  macro "hline" []

> -- | A multi row.
> multiRow    :: Int -- ^ How many rows to span
>             -> Doc -- ^ the contents
>             -> Doc

> multiRow r d = macro "multirow" [int r, char '*' , d]


> -- | A multi column.

> multiColumn :: Int      -- ^ the column width
>             -> String   -- ^ the column specification like |c| or |l| etc
>             -> Doc      -- ^ the column contents
>             -> Doc


> multiColumn c spec d = macro "multicolumn" [int c, text spec, d]




> env :: String -> [Doc] -> Doc -> Doc
> env name args body = vcat [begin, body, end]
>     where begin = macro "begin" (text name : args)
>           end   = macro "end" [text name]

> -- | Create a table (LaTeX tabular).
> table :: String      -- ^ Column specification like |c|c| etc.
>          -> [[Doc]]  -- ^ The list of rows
>          -> Doc
> table spec = env "tabular" [text spec] . tableBody

> -- | Create a long table (LaTeX longtable). Requires the use of longtable package.
> longTable :: String       -- ^ Column specification like |c|c| etc.
>              -> [[Doc]]   -- ^ The list of rows
>              -> Doc
> longTable spec = env "longtable" [text spec] . tableBody



> tableBody :: [ [Doc] ] -> Doc
> tableBody = vcat . punctuate (text " \\\\") . map row


> row :: [Doc] -> Doc
> row = hcat . punctuate (text " & ")

