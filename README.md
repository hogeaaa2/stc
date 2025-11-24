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
    