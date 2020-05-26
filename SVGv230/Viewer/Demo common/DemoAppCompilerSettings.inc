{$IFnDEF FPC}
{$IFNDEF VER230}
  {$LEGACYIFEND ON}
{$ENDIF}
{$ENDIF}

{$IFnDEF FPC}
{$IF CompilerVersion >= 23} // Delphi XE2
  {$DEFINE Ver230Up}
  {$IF CompilerVersion >= 24} // Delphi XE3
    {$DEFINE Ver240Up}
    {$IF CompilerVersion >= 25} // Delphi XE4
      {$DEFINE Ver250Up}
      {$IF CompilerVersion >= 26} // Delphi XE5
        {$DEFINE Ver260Up}
        {$IF CompilerVersion >= 27} // Delphi XE6
          {$DEFINE Ver270Up}
          {$IF CompilerVersion >= 28} // Delphi XE7
            {$DEFINE Ver280Up}
            {$IF CompilerVersion >= 29} // Delphi XE8
              {$DEFINE Ver290Up}
              {$IF CompilerVersion >= 30} // Delphi DX
                {$DEFINE Ver300Up}
                  {$IF CompilerVersion >= 31} // Delphi DX1
                    {$DEFINE Ver310Up}
                    {$IF CompilerVersion >= 32} // Delphi DX2
                      {$DEFINE Ver320Up}
                      {$IF CompilerVersion >= 33} // Delphi DX3
                        {$DEFINE Ver330Up}
                      {$IFEND}					  
                    {$IFEND}
                  {$IFEND}
               {$IFEND}
            {$IFEND}
          {$IFEND}
        {$IFEND}
      {$IFEND}
    {$IFEND}
  {$IFEND}
{$IFEND}
{$ENDIF}

{$IFnDEF FPC}
{$IF CompilerVersion <= 33} // Delphi DX3
  {$DEFINE Ver330Down}
  {$IF CompilerVersion <= 32} // Delphi DX2
    {$DEFINE Ver320Down}
    {$IF CompilerVersion <= 31} // Delphi DX1
      {$DEFINE Ver310Down}
      {$IF CompilerVersion <= 30} // Delphi DX
        {$DEFINE Ver300Down}
        {$IF CompilerVersion <= 29} // Delphi XE8
          {$DEFINE Ver290Down}
          {$IF CompilerVersion <= 28} // Delphi XE7
            {$DEFINE Ver280Down}
            {$IF CompilerVersion <= 27} // Delphi XE6
              {$DEFINE Ver270Down}
              {$IF CompilerVersion <= 26} // Delphi XE5
                {$DEFINE Ver260Down}
                {$IF CompilerVersion <= 25} // Delphi XE4
                  {$DEFINE Ver250Down}
                  {$IF CompilerVersion <= 24} // Delphi XE3
                    {$DEFINE Ver240Down}
                    {$IF CompilerVersion <= 23} // Delphi XE2
                      {$DEFINE Ver230Down}
                    {$IFEND}
                   {$IFEND}
                {$IFEND}
              {$IFEND}
            {$IFEND}
          {$IFEND}
        {$IFEND}
      {$IFEND}
    {$IFEND}
  {$IFEND}
{$IFEND}
{$ENDIF}


