# stc

## Building

1. Install [ghcup](https://www.haskell.org/ghcup/)
2. `git clone`
3. `stack build`

## Testing

* `stack test :p`: parser test
* `stack test :s`: semantic test
* `stack test`: all testcases

## FFI

After `stack build` you will get **stc-ffi.dll** in your .stack-work directory

Here's sample code for how to use this dll

```csharp        
        [DllImport("stc-ffi.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern void hs_init(IntPtr argc, IntPtr argv);

        [DllImport("stc-ffi.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern void hs_exit();

        [DllImport("stc-ffi.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "parseST")]
        public static extern IntPtr parseST(byte[] utf8, UIntPtr len);

        [DllImport("stc-ffi.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "freeInPtr")]
        public static extern void freeInPtr(IntPtr ptr);

        public static void Main(string[] args)
        {
            Console.WriteLine("Initializing runtime...");
            hs_init(IntPtr.Zero, IntPtr.Zero);

            try
            {
                var files = new[]
                {
                      new { path = "a.st", text = "PROGRAM P\nVAR x: INT; END_VAR\n;" },
                      new { path = "b.st", text = "TYPE MyInt : INT; END_TYPE\n" },
                };
                var json = JsonSerializer.Serialize(files);
                var utf8 = Encoding.UTF8.GetBytes(json);
                var p = parseST(utf8, (UIntPtr)utf8.Length);

                string res = Marshal.PtrToStringUTF8(p)!;
                Console.WriteLine(res);
                freeInPtr(p);
            }
            finally
            {
                Console.WriteLine("Exiting runtime...");
                hs_exit();
            }
        }
```

Output:

```
Initializing runtime...
{"ast":null,"ast_show":"[UProgram (Program {progName = Loc {locSpan = Span {spanStart = SourcePos {sourceName = \"a.st\", sourceLine = Pos 1, sourceColumn = Pos 9}, spanEnd = SourcePos {sourceName = \"a.st\", sourceLine = Pos 2, sourceColumn = Pos 1}}, locVal = \"P\"}, progVarDecls = VarDecls [Variable {varName = Loc {locSpan = Span {spanStart = SourcePos {sourceName = \"a.st\", sourceLine = Pos 2, sourceColumn = Pos 5}, spanEnd = SourcePos {sourceName = \"a.st\", sourceLine = Pos 2, sourceColumn = Pos 6}}, locVal = \"x\"}, varType = INT, varInit = Just (EINT 0), varKind = VKLocal, varConst = False, varRetain = False}], progBody = [Skip]}),UType [TypeDecl {typeName = Loc {locSpan = Span {spanStart = SourcePos {sourceName = \"b.st\", sourceLine = Pos 1, sourceColumn = Pos 6}, spanEnd = SourcePos {sourceName = \"b.st\", sourceLine = Pos 1, sourceColumn = Pos 12}}, locVal = \"MyInt\"}, typeBody = INT}]]","errors":[],"ok":true}
Exiting runtime...
```

Pretty-printed "ast_show" :

```haskell
[ UProgram
    ( Program
        { progName = Loc
            { locSpan = Span
                { spanStart = SourcePos
                    { sourceName = "a.st", sourceLine = Pos 1, sourceColumn = Pos 9
                    }, spanEnd = SourcePos
                    { sourceName = "a.st", sourceLine = Pos 2, sourceColumn = Pos 1
                    }
                }, locVal = "P"
            }, progVarDecls = VarDecls
            [ Variable
                { varName = Loc
                    { locSpan = Span
                        { spanStart = SourcePos
                            { sourceName = "a.st", sourceLine = Pos 2, sourceColumn = Pos 5
                            }, spanEnd = SourcePos
                            { sourceName = "a.st", sourceLine = Pos 2, sourceColumn = Pos 6
                            }
                        }, locVal = "x"
                    }, varType = INT, varInit = Just
                    ( EINT 0 ), varKind = VKLocal, varConst = False, varRetain = False
                }
            ], progBody = [ Skip ]
        }
    ), UType
    [ TypeDecl
        { typeName = Loc
            { locSpan = Span
                { spanStart = SourcePos
                    { sourceName = "b.st", sourceLine = Pos 1, sourceColumn = Pos 6
                    }, spanEnd = SourcePos
                    { sourceName = "b.st", sourceLine = Pos 1, sourceColumn = Pos 12
                    }
                }, locVal = "MyInt"
            }, typeBody = INT
        }
    ]
]
```