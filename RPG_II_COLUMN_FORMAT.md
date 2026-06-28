# RPG II Column Format Reference

## Standard IBM RPG II Fixed-Column Format

RPG II uses a strict fixed-column format where each specification type has specific column positions for each field.

### Calculation Specification (C-Spec)
```
Columns  Field
------   -----
1-2      Form Type (blank)
3-5      Page/Line (blank)
6        Form Type (C)
7-8      Control Level (L1-L9, LR) or Indicators (01-99)
9-17     Factor 1
18-27    Operation Code
28-32    Factor 2
33-42    Result Field
43-48    Field Length (43-45=length, 46-48=decimal positions)
49-51    Resulting Indicators (Plus/Minus/Zero or Error)
52-59    Resulting Indicators (continued)
60-74    Comments
```

### Output Specification (O-Spec)
```
Columns  Field
------   -----
1-2      Form Type (blank)
3-5      Page/Line (blank)
6        Form Type (O)
7-14     File Name
15       Type (H=Heading, D=Detail, T=Total)
16       Space (1-3) or Skip (01-99)
17-18    Skip (continued)
19-20    Output Indicators
21-29    Field Name or EXCEPT name
30       Edit Code
31       Blank After
32       End Position (start)
33-34    End Position (middle)
35-40    End Position (end) - rightmost column for output
41-70    Constant (if no field name)
```

### File Description Specification (F-Spec)
```
Columns  Field
------   -----
1-2      Form Type (blank)
3-5      Page/Line (blank)
6        Form Type (F)
7-14     File Name
15       File Type (I=Input, O=Output, U=Update, C=Combined)
16       File Designation (P=Primary, S=Secondary, R=Record Address, T=Table, blank=Full Procedural)
17       End of File (E=End of File)
18       File Format (F=Fixed, V=Variable)
19-22    Block Length
23       Record Length Type (blank or F)
24-27    Record Length
28-32    Mode of Processing (blank)
33       Length of Key Field
34-38    Record Address Type
39-46    Device (DISK, PRINTER, WORKSTN, etc.)
47-52    Symbolic Device
53       Continuation (K for continuation)
54-59    Continuation lines
60-65    Option Entry
66-70    Keywords
71-74    Device Name
```

### Key Points

1. **All columns are 1-based** (column 1 is the first character)
2. **Spaces matter** - blank columns have meaning
3. **Right-justified numeric fields** - numbers align to the right within their column range
4. **Left-justified alpha fields** - text aligns to the left
5. **No variations allowed** - the format is absolute

### Common Mistakes

1. **C-Spec Operation Code**: Must be in columns 18-27 (10 characters)
2. **C-Spec Result Field**: Must be in columns 33-42 (10 characters)
3. **C-Spec Field Length**: Columns 43-48 (length in 43-45, decimals in 46-48)
4. **O-Spec End Position**: Must be in columns 40-44 (right-justified)
5. **O-Spec Constants**: Must be in columns 45-70 with quotes

### Example: Proper C-Spec Format
```
      C           Z-ADD100         NUM1     52
Cols: 123456789012345678901234567890123456789012345678
      C     Factor1  OpCode Factor2  Result  Len
```

### Example: Proper O-Spec Format
```
      O          D    1
      O                                  40 'TEXT'
Cols: 123456789012345678901234567890123456789012345678901234567890
      O  Filename T Sp Field/Const      EndPos Constant