unit RegularExpressions;

{ .Net-style Regular Expression Library for use with Delphi 2009 or later.
  Copyright (C) 2009 by Erik van Bilsen.
  Email: erik@bilsen.com
  Website: www.bilsen.com/regularexpressions

License in plain English:

1. I don't promise that this software works. (But if you find any bugs,
   please let me know!)
2. You can use this software for whatever you want. You don't have to pay me.
3. You may not pretend that you wrote this software. If you use it in a program,
   you must acknowledge somewhere in your documentation that you've used this
   code.

In legalese:

The author makes NO WARRANTY or representation, either express or implied,
with respect to this software, its quality, accuracy, merchantability, or
fitness for a particular purpose.  This software is provided "AS IS", and you,
its user, assume the entire risk as to its quality and accuracy.

Permission is hereby granted to use, copy, modify, and distribute this
software (or portions thereof) for any purpose, without fee, subject to these
conditions:
(1) If any part of the source code for this software is distributed, then the
License.txt file must be included, with this copyright and no-warranty notice
unaltered; and any additions, deletions, or changes to the original files
must be clearly indicated in accompanying documentation.
(2) If only executable code is distributed, then the accompanying
documentation must state that "this software is based in part on the Regular
Expression Library by Erik van Bilsen".
(3) Permission for use of this software is granted only if the user accepts
full responsibility for any undesirable consequences; the author accepts
NO LIABILITY for damages of any kind. }

{$IF RTLVersion < 20}
  {$MESSAGE Error 'This unit requires Delphi 2009 or later'}
{$IFEND}

{$WARN SYMBOL_PLATFORM OFF}

interface

{ See ReadMe.htm file for information on this library }

uses
  Classes, SysUtils,

  pcre;

type
  TStringArray = array of UTF8String;
  TIntegerArray = array of Integer;

type
  ERegexError = class(Exception);

type
  TRegexOption = (roIgnoreCase, roMultiline, roExplicitCapture, roSingleline,
    roIgnorePatternWhitespace, roStudy, roSkipEmptyMatches);

  TRegexOptions = set of TRegexOption;

type
  IMatch = interface;
  IMatchCollection = interface;

  TMatchEvaluator = function(const Match: IMatch): String of Object;

  { The IRegex class represents the .NET-like regular expression engine. It can
    be used to quickly parse large amounts of text to find specific character
    patterns; to extract, edit, replace, or delete text substrings; or to add
    the extracted strings to a collection in order to generate a report.

    This interface is implemented in the TRegex class. That class also contains
    several class static methods that allow you to use a regular expression
    without explicitly creating a TRegex object.
    Regular expressions compiled from class static method calls are cached,
    whereas regular expressions compiled from instance method calls are not
    cached. By default, the regular expression engine caches the 15 most
    recently used static regular expressions. As a result, in applications that
    rely extensively on a fixed set of regular expressions to extract, modify,
    or validate text, you may prefer to call these static methods rather than
    their corresponding instance methods. Class static overloads of the IsMatch,
    Match, Matches, Replace, and Split methods are available.

    Note: If the default cache size of 15 static regular expressions is
    inadequate for your application, you can increase it by modifying the value
    of the static CacheSize property of the TRegex class. }
  IRegex = interface
  ['{5E3C2BCA-56C8-46DE-959F-338AF5F69C1A}']
    // Property access methods
    function GetOptions: TRegexOptions;

    // Methods

    { Returns an array of capturing group names for the regular expression.
      The collection of group names contains the set of strings used to name
      capturing groups in the expression. Even if capturing groups are not
      explicitly named, they are automatically assigned numerical names (1, 2,
      3, and so on). Therefore, this collection can be used to determine the
      number of groups. }
    function GetGroupNames: TStringArray;

    { Returns an array of capturing group numbers that correspond to group names
      in an array. Referencing a group by its number instead of by string name
      can provide faster access. }
    function GetGroupNumbers: TIntegerArray;

    { Gets the group name that corresponds to the specified group number.
      A regular expression pattern may contain either named or numbered
      capturing groups, which delineate subexpressions within a pattern match.
      Numbered groups are delimited by the syntax (subexpression) and are
      assigned numbers based on their order in the regular expression. Named
      groups are delimited by the syntax (?<name>subexpression) or
      (?'name'subexpression), where name is the name by which the subexpression
      will be identified. The GroupNameFromNumber method identifies both named
      groups and numbered groups by their ordinal positions in the regular
      expression. Ordinal position zero always represents the entire regular
      expression. All numbered groups are then counted before named groups,
      regardless of their actual position in the regular expression pattern.

      If a pattern match is successful, the value returned by this method can
      then be used to retrieve the IGroup object that represents the captured
      group from the IGroupCollection.Items property. The IGroupCollection
      object is returned by the IMatch.Groups property. }
    function GroupNameFromNumber(const I: Integer): UTF8String;

    { Returns the group number that corresponds to the specified group name. }
    function GroupNumberFromName(const Name: UTF8String): Integer;

    { Indicates whether the regular expression finds a match in the input string.
      The IsMatch method is typically used to validate a string or to ensure
      that a string conforms to a particular pattern without retrieving that
      string for subsequent manipulation. To determine whether one or more
      strings match a regular expression pattern and to retrieve them for
      subsequent manipulation, call the Match or Matches method.

      You can optionally supply a StartAt parameter to start the search at
      a specific character position (1 to start from the first character) }
    function IsMatch(const Input: UTF8String): Boolean; overload;
    function IsMatch(const Input: UTF8String; const StartAt: Integer): Boolean; overload;

    { Searches an input string for a substring that matches a regular expression
      pattern and returns the first occurrence as a single IMatch object.
      You can determine whether the regular expression pattern has been found in
      the input string by checking the value of the returned IMatch object's
      Success property. If a match is successful, the returned IMatch object's
      Value property contains the substring from input that matches the regular
      expression pattern. If no match is found, its value is an empty string.

      This method returns the first substring in input that matches the regular
      expression pattern. You can retrieve subsequent matches by repeatedly
      calling the returned IMatch object's NextMatch method. You can also
      retrieve all matches in a single method call by calling the
      IRegex.Matches method.

      You can optionally supply a StartAt parameter to start the search at a
      specific character position (1 to start from the first character).
      You can also optionally supply a maximum number of characters to search. }
    function Match(const Input: UTF8String): IMatch; overload;
    function Match(const Input: UTF8String; const StartAt: Integer): IMatch; overload;
    function Match(const Input: UTF8String; const Beginning, Length: Integer): IMatch; overload;

    { Searches an input string for all occurrences of a regular expression and
      returns all the successful matches.
      The Matches methods are similar to the Match methods, except they return
      the list of successful matches that would result from iteratively calling
      IMatch.NextMatch. The collection includes only successful matches and
      terminates at the first unsuccessful match.

      You can optionally supply a StartAt parameter to start the search at a
      specific character position (1 to start from the first character). }
    function Matches(const Input: UTF8String): IMatchCollection; overload;
    function Matches(const Input: UTF8String; const StartAt: Integer): IMatchCollection; overload;

    { Within a specified input string, replaces strings that match a regular
      expression pattern with a specified replacement string.

      You can either specify a replacement string or an event that gets fired
      for each replacement. The event is usefule if:
      -The replacement string cannot readily be specified by a regular
       expression replacement pattern.
      -The replacement string results from some processing done on the matched
       string.
      -The replacement string results from conditional processing.

      You can optionally supply the following parameters:
      -Count: Maximum number of times the replacement can occur.
      -StartAt: The character position in the input string where the search
       begins (1 for the first character). }
    function Replace(const Input, Replacement: UTF8String): UTF8String; overload;
    function Replace(const Input: UTF8String;
      const Evaluator: TMatchEvaluator): UTF8String; overload;
    function Replace(const Input, Replacement: UTF8String;
      const Count: Integer): UTF8String; overload;
    function Replace(const Input: UTF8String; const Evaluator: TMatchEvaluator;
      const Count: Integer): UTF8String; overload;
    function Replace(const Input, Replacement: UTF8String;
      const Count, StartAt: Integer): UTF8String; overload;
    function Replace(const Input: UTF8String; const Evaluator: TMatchEvaluator;
      const Count, StartAt: Integer): UTF8String; overload;

    { Splits an input string into an array of substrings at the positions
      defined by a regular expression match. The string is split as many times
      as possible. If no delimiter is found, the return value contains one
      element whose value is the original input parameter string.

      If multiple matches are adjacent to one another, an empty string is
      inserted into the array. For example, splitting a string on a single
      hyphen causes the returned array to include an empty string in the
      position where two adjacent hyphens are found.

      If capturing parentheses are used in a IRegex.Split expression, any
      captured text is included in the resulting string array. For example,
      splitting the string 'plum-pear' on a hyphen placed within capturing
      parentheses adds a string element that contains the hyphen to the
      returned array.

      You can optionally supply the following parameters:
      -Count: Maximum number of times a split can occur.
      -StartAt: The character position in the input string where the search
       begins (1 for the first character). }
    function Split(const Input: UTF8String): TStringArray; overload;
    function Split(const Input: UTF8String; const Count: Integer): TStringArray; overload;
    function Split(const Input: UTF8String; const Count, StartAt: Integer): TStringArray; overload;

    { Returns the regular expression pattern that was passed into the TRegex
      constructor. }
    function ToString: String;

    { Saves the compiled intermediate version of the regular expression to a
      stream. When the regular expression is also studies (using the roStudy
      option in the constructor), the study result will also be saved to the
      stream. See the Load method for information how to load the regular
      expression again. }
    procedure Save(const Stream: TStream);

    { Loads a regular expression saved with the Save method. The regex is
      loaded from the current position in the stream.
      To create a regex from a stream, you usually create an empty IRegex
      object (by passing no parameters to the TRegex constructor), followed
      by a call to Load.
      NOTE: The regex stream contains version information. You can only load
      regex's that are saved with the same version of this library, or with
      an older version. }
    procedure Load(const Stream: TStream);

    // Properties

    { Returns the options passed into the TRegex constructor. }
    property Options: TRegexOptions read GetOptions;
  end;

  { Internal interfaced used to enumerate over regular expression collections
    (groups and matches) }
  IEnumerator<T> = interface
  ['{60DA28C0-6D08-4FE1-9B23-0731E5EC079C}']
    // Property access methods
    function GetCurrent: T;

    // Methods
    function MoveNext: Boolean;

    // Properties
    property Current: T read GetCurrent;
  end;

  { Base interface for regular expression collections (groups and matches) }
  ICollection<T> = interface
  ['{1B816C95-08EB-41DC-990B-A1904973B80C}']
    // Property access methods
    function GetIsReadOnly: Boolean;
    function GetIsSynchronized: Boolean;
    function GetSyncRoot: TObject;
    function GetCount: Integer;
    function GetItem(const Index: Integer): T;

    // Methods

    { Support for the for..in enumerator }
    function GetEnumerator: IEnumerator<T>;

    // Properties

    { Gets a value indicating whether the collection is read-only.
      This always returns True for regular expression collections. }
    property IsReadOnly: Boolean read GetIsReadOnly;

    { Gets a value indicating whether access to the collection is synchronized
      (thread-safe).
      This always returns False for regular expression collections. }
    property IsSynchronized: Boolean read GetIsSynchronized;

    { Gets an object that can be used to synchronize access to the collection.
      (Using TMonitor.Enter and TMonitor.Exit) }
    property SyncRoot: TObject read GetSyncRoot;

    { Returns the number of items in the collection. }
    property Count: Integer read GetCount;

    { Enables access to a member of the collection by integer index. }
    property Items[const Index: Integer]: T read GetItem; default;
  end;

  { Base interface for the IMatch and IGroup interfaces. }
  ICapture = interface
  ['{2D3FF0DA-A74D-4D76-8276-39554794FBAB}']
    // Property access methods
    function GetIndex: Integer;
    function GetLength: Integer;
    function GetValue: UTF8String;

    // Methods

    { Gets the captured substring from the input string.
      (Returns the same string as the Value property) }
    function ToString: String;

    // Properties

    { The position in the original string where the first character of the
      captured substring was found.
      This is a 1-based position (1 indicates the first character in the
      input string) }
    property Index: Integer read GetIndex;

    { The length of the captured substring. }
    property Length: Integer read GetLength;

    { Gets the captured substring from the input string. }
    property Value: UTF8String read GetValue;
  end;

  { IGroup represents the results from a single capturing group.
    It inherits properties and methods from ICapture. }
  IGroup = interface(ICapture)
  ['{4C8E4835-6D43-4C80-AF12-1FB1CF68D6E7}']
    // Property access methods
    function GetSuccess: Boolean;

    // Properties

    { Gets a value indicating whether the match is successful.
      The Success property is true if at least one substring was captured by
      this group. }
    property Success: Boolean read GetSuccess;
  end;

  { Represents a collection of captured groups. IGroupCollection returns the set
    of captured groups in a single match.
    It inherits properties and methods from ICollection<T>.

    The collection is immutable (read-only). An IGroupCollection object is
    returned by the IMatch.Groups property.

    The collection contains one or more IGroup objects. The first element in the
    collection contains the string that corresponds to the entire match. Each
    subsequent element represents a captured group, if the regular expression
    includes capturing groups. }
  IGroupCollection = interface(ICollection<IGroup>)
  ['{CB7547F0-B797-45EB-9EB7-6F7070918DBE}']
    // Property access methods
    function GetItemByName(const GroupName: UTF8String): IGroup;

    // Properties

    { Enables access to a member of the collection by string index.
      The IGroupCollection object returned by the IMatch.Groups property always
      has at least one member. If the regular expression engine cannot find any
      matches in a particular input string, the single IGroup object in the
      collection has its IGroup.Success property set to False and its
      IGroup.Value property set to an empty string. }
    property ItemsByName[const GroupName: UTF8String]: IGroup read GetItemByName;
  end;

  { Represents the results from a single regular expression match.
    It inherits properties and methods from IGroup and ICapture.

    An IMatch object is returned by the IRegex.Match method and represents the
    first pattern match in a string. Subsequent matches are represented by
    IMatch objects returned by the IMatch.NextMatch method. In addition, an
    IMatchCollection object that consists of zero, one, or more IMatch objects
    is returned by the IRegex.Matches method.

    If the IRegex.Matches method fails to match a regular expression pattern in
    an input string, it returns an empty IMatchCollection object. You can then
    use a for..in construct to iterate the collection.

    If the IRegex.Match method fails to match the regular expression pattern, it
    returns an IMatch object that is equal to TMatch.Empty. You can use the
    Success property to determine whether the match was successful.

    If a pattern match is successful, the Value property contains the matched
    substring, the Index property indicates the one-based starting position of
    the matched substring in the input string, and the Length property indicates
    the length of matched substring in the input string.

    Because a single match can involve multiple capturing groups, IMatch has a
    Groups property that returns the IGroupCollection. The IGroupCollection has
    accessors that return each group. IMatch inherits from IGroup so the entire
    substring matched can be accessed directly. That is, the IMatch instance
    itself is equivalent to Match.Groups[0]. }
  IMatch = interface(IGroup)
  ['{6F221769-F7EA-4100-A0F7-EA692AF04CAC}']
    // Property access methods
    function GetGroups: IGroupCollection;

    // Methods

    { Returns a new IMatch object with the results for the next match, starting
      at the position at which the last match ended (at the character after the
      last matched character). }
    function NextMatch: IMatch;

    { Returns the expansion of the specified replacement pattern.
      For example, if the replacement parameter is '$1$2', the Result method
      returns the concatenation of Groups[1].Value and Groups[2].Value. }
    function Result(const Replacement: UTF8String): UTF8String;

    // Properties

    (*Gets a collection of groups matched by the regular expression.
      A regular expression pattern can include subexpressions, which are defined
      by enclosing a portion of the regular expression pattern in parentheses.
      Every such subexpression forms a group. For example, the regular
      expression pattern (\d{3})-(\d{3}-\d{4}), which matches North American
      telephone numbers, has two subexpressions. The first consists of the area
      code, which composes the first three digits of the telephone number. This
      group is captured by the first portion of the regular expression, (\d{3}).
      The second consists of the individual telephone number, which composes the
      last seven digits of the telephone number. This group is captured by the
      second portion of the regular expression, (\d{3}-\d{4}). These two groups
      can then be retrieved from the IGroupCollection object that is returned by
      the Groups property.

      The IGroupCollection object returned by the IMatch.Groups property always
      has at least one member. If the regular expression engine cannot find any
      matches in a particular input string, the IGroup.Success property of the
      single IGroup object in the collection is set to False and the IGroup
      object's Value property is set to an empty string. If the regular
      expression engine can find a match, the first element of the
      IGroupCollection object returned by the Groups property contains a string
      that matches the entire regular expression pattern.*)
    property Groups: IGroupCollection read GetGroups;
  end;

  { Represents the set of successful matches found by iteratively applying a
    regular expression pattern to the input string.
    It inherits properties and methods from ICollection<T>.

    The collection is immutable (read-only). The IRegex.Matches method returns
    an IMatchCollection object. }
  IMatchCollection = interface(ICollection<IMatch>)
  ['{73748AA2-5E8D-439F-94FE-46DD4A11054E}']
  end;

type
  { The TRegex class implements the IRegex interface.
    Never store references in variables of type TRegex, but always use IRegex.
    That will enable automatic memory management of the regular expression an
    all its sub-objects (like Matches and Groups).

    Alternatively, you can use the class static methods IsMatch, Match,
    Matches, Replace and Split. Regular expressions compiled from these class
    static method calls are cached, whereas regular expressions compiled from
    instance method calls are not cached.}
  TRegex = class(TInterfacedObject, IRegex)
  {$REGION 'Internal declarations'}
  private
    type
      TCachedCompiledPattern = class
      private
        FKey: UTF8String;
        FCompiledPattern: Pointer;
        FStudyData: Pointer;
      public
        constructor Create(const AKey: UTF8String; const ACompiledPattern,
          AStudyData: Pointer);
        destructor Destroy; override;
      end;
    type
      TGroupMapEntry = record
        Name: UTF8String;
        CaptureIndex: Integer;
      end;
    type
      TAnsiStringBuilder = class
      private
        FBuffer: PAnsiChar;
        FCurrent: PAnsiChar;
        FLength: Integer;
        FCapacity: Integer;
        procedure Grow(const Size: Integer);
        procedure Append(const Buffer: Pointer; const Size: Integer); overload;
      public
        constructor Create;
        destructor Destroy; override;
        procedure Clear;
        procedure Append(const Value: AnsiChar); overload; inline;
        procedure Append(const Value: UTF8String); overload; inline;
        procedure Append(const Value: UTF8String; const Index,
          Count: Integer); overload; inline;
        function GetValue: UTF8String;
      end;
  private
    FOptions: TRegexOptions;
    FExecFlags: Integer;
    FInternalOptions: Integer;
    FPattern: UTF8String;
    FSubject: UTF8String;
    FReplacement: UTF8String;
    FStartOffset: Integer;
    FMaxLength: Integer;
    FCompiledPattern: PPCRE;
    FGroupMap: array of TGroupMapEntry;
    FStudyData: PPCREExtra;
    FCached: Boolean;
    FOffsets: array of Integer;
    FOffsetCount: Integer;
    FEvaluator: TMatchEvaluator;
    FReplacementBuilder: TAnsiStringBuilder;
  private
    class var FCacheSize: Integer;
    class var FCache: array of TCachedCompiledPattern;
    class procedure StaticInitialize; static;
    class procedure StaticFinalize; static;
    class procedure SetCacheSize(const Value: Integer); static;
    class function LookupCache(const Key: UTF8String;
      out CompiledPattern, StudyData: Pointer): Boolean; static;
    class procedure AddToCache(const Key: UTF8String;
      const CompiledPattern, StudyData: Pointer); static;
  private
    constructor Create(const APattern: UTF8String; const AOptions: TRegexOptions;
      const AUseCache: Boolean); overload;
    procedure SetSubject(const Value: UTF8String);
    procedure Compile;
    procedure Study;
    procedure Clear;
    procedure CalculateGroupMap;
    function Match: Boolean; overload;
    function MatchNext: Boolean;
    function Replace: UTF8String; overload;
    function ExpandSubstitutions(const Replacement: UTF8String): UTF8String;
    function GetMatchIndex: Integer;
    function GetMatchLength: Integer;
  private
    class function ConvertOptions(const Options: TRegexOptions): Integer; static;
  {$ENDREGION 'Internal declarations'}
  private
    { IRegex
      See the IRegex interface for an explanation of these methods. }
    function GetOptions: TRegexOptions;

    function GetGroupNames: TStringArray;
    function GetGroupNumbers: TIntegerArray;
    function GroupNameFromNumber(const I: Integer): UTF8String;
    function GroupNumberFromName(const Name: UTF8String): Integer;

    function IsMatch(const Input: UTF8String): Boolean; overload;
    function IsMatch(const Input: UTF8String; const StartAt: Integer): Boolean; overload;
    function Match(const Input: UTF8String): IMatch; overload;
    function Match(const Input: UTF8String; const StartAt: Integer): IMatch; overload;
    function Match(const Input: UTF8String; const Beginning, Length: Integer): IMatch; overload;
    function Matches(const Input: UTF8String): IMatchCollection; overload;
    function Matches(const Input: UTF8String; const StartAt: Integer): IMatchCollection; overload;

    function Replace(const Input, Replacement: UTF8String): UTF8String; overload;
    function Replace(const Input: UTF8String;
      const Evaluator: TMatchEvaluator): UTF8String; overload;
    function Replace(const Input, Replacement: UTF8String;
      const Count: Integer): UTF8String; overload;
    function Replace(const Input: UTF8String; const Evaluator: TMatchEvaluator;
      const Count: Integer): UTF8String; overload;
    function Replace(const Input, Replacement: UTF8String;
      const Count, StartAt: Integer): UTF8String; overload;
    function Replace(const Input: UTF8String; const Evaluator: TMatchEvaluator;
      const Count, StartAt: Integer): UTF8String; overload;

    function Split(const Input: UTF8String): TStringArray; overload;
    function Split(const Input: UTF8String; const Count: Integer): TStringArray; overload;
    function Split(const Input: UTF8String; const Count, StartAt: Integer): TStringArray; overload;
  public
    { Creates an empty IRegex object. This version is only useful if you plan
      to load the regular expression later using the Load method. }
    constructor Create; overload;

    { Initializes an IRegex object for the specified regular expression.
      The pattern parameter consists of various regular expression language
      elements that symbolically describe the string to match. For more
      information about regular expressions, see
      http://msdn.microsoft.com/en-us/library/hs600312.aspx

      An IRegex object is immutable, which means that it can be used only for
      the match parameters defined when it is created. It can be used any number
      of times without being recompiled, however.

      If you don't specify the AOptions parameter, the constructor instantiates
      a regular expression object that performs a case-sensitive match of any
      alphabetic characters defined in pattern.

      You can optionally supply the following options in the AOptions parameter:
      -roIgnoreCase: Specifies case-insensitive matching.
      -roMultiline: Multiline mode. Changes the meaning of ^ and $ so they match
       at the beginning and end, respectively, of any line, and not just the
       beginning and end of the entire string.
      -roExplicitCapture: Specifies that the only valid captures are explicitly
       named or numbered groups of the form (?<name>…). This allows unnamed
       parentheses to act as noncapturing groups without the syntactic
       clumsiness of the expression (?:…).
      -roSingleline: Specifies single-line mode. Changes the meaning of the dot
       (.) so it matches every character (instead of every character except a
       line break).
      -roIgnorePatternWhitespace: Eliminates unescaped white space from the
       pattern. However, the roIgnorePatternWhitespace value does not affect or
       eliminate white space in character classes.
      -roStudy: This will cause a more in-depth analysis of the regex pattern,
       which may speedup the matching process. This is especially useful if you
       use the same pattern multiple times. The analysis takes some time too, so
       you should only use this option when needed. }
    constructor Create(const APattern: UTF8String); overload;
    constructor Create(const APattern: UTF8String; const AOptions: TRegexOptions); overload;

    destructor Destroy; override;

    { Escapes a minimal set of characters (\, *, +, ?, |, {, [, (,), ^, $,., #,
      and white space) by replacing them with their escape codes. This instructs
      the regular expression engine to interpret these characters literally
      rather than as metacharacters. }
    class function Escape(const Str: UTF8String): UTF8String; static;

    (*Converts any escaped characters in the input string.
      The Unescape method performs one of the following two transformations:
      -It reverses the transformation performed by the Escape method by removing
       the escape character ("\") from each character escaped by the method.
       These include the \, *, +, ?, |, {, [, (,), ^, $,., #, and white space
       characters. In addition, the Unescape method unescapes the closing
       bracket (]) and closing brace (}) characters.
       Note: Unescape cannot reverse an escaped string perfectly because it
       cannot deduce precisely which characters were escaped,
      -It replaces the representation of unprintable characters with the
       characters themselves. For example, it replaces \a with #$07. The
       character representations it replaces are \a, \b, \e, \n, \r, \f, \t,
       and \v.*)
    class function Unescape(const Str: UTF8String): UTF8String; static;

    { See the IRegex interface for an explanation of these methods. }

    class function IsMatch(const Input, Pattern: UTF8String): Boolean; overload; static;
    class function IsMatch(const Input, Pattern: UTF8String;
      const Options: TRegexOptions): Boolean; overload; static;
    class function Match(const Input, Pattern: UTF8String): IMatch; overload; static;
    class function Match(const Input, Pattern: UTF8String; StartAt: Integer): IMatch; overload; static;
    class function Match(const Input, Pattern: UTF8String;
      const Options: TRegexOptions): IMatch; overload; static;
    class function Match(const Input, Pattern: UTF8String;
      const Options: TRegexOptions; StartAt, Count: Integer): IMatch; overload; static;
    class function Matches(const Input, Pattern: UTF8String): IMatchCollection; overload; static;
    class function Matches(const Input, Pattern: UTF8String; StartAt: Integer): IMatchCollection; overload; static;
    class function Matches(const Input, Pattern: UTF8String;
      const Options: TRegexOptions): IMatchCollection; overload; static;
    class function Matches(const Input, Pattern: UTF8String;
      const Options: TRegexOptions; StartAt: Integer): IMatchCollection; overload; static;

    class function Replace(const Input, Pattern, Replacement: UTF8String): UTF8String; overload; static;
    class function Replace(const Input, Pattern: UTF8String;
      const Evaluator: TMatchEvaluator): UTF8String; overload; static;
    class function Replace(const Input, Pattern, Replacement: UTF8String;
      const Options: TRegexOptions): UTF8String; overload; static;
    class function Replace(const Input, Pattern: UTF8String;
      const Evaluator: TMatchEvaluator; const Options: TRegexOptions): UTF8String; overload; static;

    class function Split(const Input, Pattern: UTF8String): TStringArray; overload; static;
    class function Split(const Input, Pattern: UTF8String;
      const Options: TRegexOptions): TStringArray; overload; static;

    function ToString: String; override;
    procedure Save(const Stream: TStream);
    procedure Load(const Stream: TStream);

    class property CacheSize: Integer read FCacheSize write SetCacheSize;
  end;

type
  { Implements the ICapture interface. }
  TCapture = class(TInterfacedObject, ICapture)
  {$REGION 'Internal declarations'}
  private
    FValue: UTF8String;
    FIndex: Integer;
    FLength: Integer;
    constructor Create(const AValue: UTF8String; const AIndex, ALength: Integer);
  {$ENDREGION 'Internal declarations'}
  private
    { ICapture
      See the ICapture interface for an explanation of these methods. }
    function GetIndex: Integer;
    function GetLength: Integer;
    function GetValue: UTF8String;
  public
    function ToString: String; override;
  end;

type
  { Implements the IGroup interface. }
  TGroup = class(TCapture, IGroup)
  {$REGION 'Internal declarations'}
  private
    class var FEmptyGroup: IGroup;
    class function GetEmptyGroup: IGroup; static;
    class property EmptyGroup: IGroup read GetEmptyGroup;
  {$ENDREGION 'Internal declarations'}
  private
    { IGroup
      See the IGroup interface for an explanation of this method. }
    function GetSuccess: Boolean;
  end;

type
  { Implements the IMatch interface. }
  TMatch = class(TGroup, IMatch)
  {$REGION 'Internal declarations'}
  private
    class var FEmpty: IMatch;
  private
    FGroups: IGroupCollection;
    FRegex: TRegex;
    FRegexIntf: IRegex;
    class function GetEmpty: IMatch; static;
    constructor Create(const ARegex: TRegex; const AValue: UTF8String;
      const AIndex, ALength: Integer);
  {$ENDREGION 'Internal declarations'}
  private
    { IMatch
      See the IMatch interface for an explanation of these methods. }
    function GetGroups: IGroupCollection;
    function NextMatch: IMatch;
    function Result(const Replacement: UTF8String): UTF8String;
  public
    { Gets the empty group. All failed matches return this empty match.
      This property should not be used to determine if a match is successful.
      Use Success instead. }
    class property Empty: IMatch read GetEmpty;
  end;

resourcestring
  RS_COMPILE_ERROR = 'Error in regular expression at offset %d: %s';
  RS_STUDY_ERROR = 'Error studying the regular expression: %s';
  RS_ARGUMENT_OUT_OF_RANGE = 'Argument out of range';
  RS_ARGUMENT_NIL = 'Argument cannot be nil';
  RS_ARGUMENT_EMPTY = 'Argument cannot be empty';
  RS_TOO_FEW_HEX_CHARS = 'Invalid escape code: too few hex chars';
  RS_MISSING_CONTROL = 'Invalid escape code: missing control character';
  RS_UNRECOGNIZED_CONTROL = 'Invalid escape code: unrecognized control character';
  RS_INVALID_SIGNATURE = 'Invalid signature in regex stream';
  RS_UNSUPPORTED_VERSION = 'Unsupported version of regex stream';

implementation

uses
  Windows,
  AnsiStrings,
  RTLConsts;

type
  TCollection<T> = class(TInterfacedObject, ICollection<T>)
  {$REGION 'Internal declarations'}
  private
    type
      TEnumerator = class(TInterfacedObject, IEnumerator<T>)
      private
        FCollection: TCollection<T>;
        FIndex: Integer;
        constructor Create(const ACollection: TCollection<T>);
        function GetCurrent: T;
      public
        property Current: T read GetCurrent;
        function MoveNext: Boolean;
      end;
  private
    FItems: array of T;
    FCount: Integer;
    FCapacity: Integer;
    procedure Grow;
  private
    procedure Add(const Value: T);
  {$ENDREGION 'Internal declarations'}
  public
    { ICollection<T> }
    function GetIsReadOnly: Boolean;
    function GetIsSynchronized: Boolean;
    function GetSyncRoot: TObject; virtual; abstract;
    function GetCount: Integer;
    function GetItem(const Index: Integer): T;
    function GetEnumerator: IEnumerator<T>;
  end;

type
  TGroupCollection = class(TCollection<IGroup>, IGroupCollection)
  {$REGION 'Internal declarations'}
  private
    FMatch: TMatch;
  {$ENDREGION 'Internal declarations'}
  public
    { ICollection }
    function GetSyncRoot: TObject; override;
  public
    { IGroupCollection }
    function GetItemByName(const GroupName: UTF8String): IGroup;
  public
    constructor Create(const AMatch: TMatch);
  end;

type
  TMatchCollection = class(TCollection<IMatch>, IMatchCollection)
  public
    { ICollection }
    function GetSyncRoot: TObject; override;
  end;

type
  TRegexHeader = packed record
    Signature: array [0..3] of AnsiChar;
    Version: Longint;
    CompiledSize: Longint;
    StudyDataSize: Longint;
  end;

const
  REGEX_SIGNATURE = 'PCRE';
  REGEX_VERSION   = 1;

{ Returns the number of bytes a single Unicode character takes, given the lead
  byte of a UTF-8 encoded string }
function UTF8ByteCount(const LeadByte: Byte): Integer; overload; inline;
begin
  if (LeadByte < $80) then
    Result := 1
  else if (LeadByte < $E0) then
    Result := 2
  else if (LeadByte < $F0) then
    Result := 3
  else
    Result := 4;
end;

function UTF8ByteCount(const LeadChar: AnsiChar): Integer; overload; inline;
begin
  Result := UTF8ByteCount(Byte(LeadChar));
end;

{ TRegex }

class procedure TRegex.AddToCache(const Key: UTF8String; const CompiledPattern,
  StudyData: Pointer);
var
  I: Integer;
  Entry: TCachedCompiledPattern;
begin
  for I := 0 to Length(FCache) - 1 do
  begin
    Entry := FCache[I];
    if Assigned(Entry) and (Entry.FKey = Key) then
    begin
      if (I > 0) then
      begin
        Move(FCache[0], FCache[1], I * SizeOf(TCachedCompiledPattern));
        FCache[0] := Entry;
      end;
      Exit;
    end;
  end;

  if (FCacheSize <> 0) then
  begin
    Entry := FCache[Length(FCache) - 1];
    Entry.Free;

    Move(FCache[0], FCache[1], (Length(FCache) - 1) * SizeOf(TCachedCompiledPattern));
    FCache[0] := TCachedCompiledPattern.Create(Key, CompiledPattern, StudyData);
  end;
end;

procedure TRegex.CalculateGroupMap;
var
  GroupCount, NameCount, NameEntrySize, Index: Integer;
  GroupIndex, TargetIndex: Integer;
  NameTable, P: PAnsiChar;
  IsNamed: array of Integer;
begin
  { In .Net, capture groups always start with the numbered groups, followed by
    the named groups.
    In PCRE, capture groups are in capture order.
    This method maps the PCRE order to the .Net order. It also records
    the name of each group, so the GroupNameFromNumber method can retrieve
    these later. }

  pcre_fullinfo(FCompiledPattern, nil, PCRE_INFO_CAPTURECOUNT, @GroupCount);
  pcre_fullinfo(FCompiledPattern, nil, PCRE_INFO_NAMECOUNT, @NameCount);
  pcre_fullinfo(FCompiledPattern, nil, PCRE_INFO_NAMEENTRYSIZE, @NameEntrySize);
  pcre_fullinfo(FCompiledPattern, nil, PCRE_INFO_NAMETABLE, @NameTable);

  SetLength(FOffsets, (GroupCount + 1) * 3 + 4);

  { For each named group, the NameTable contains an entry of NameEntrySize
    bytes. The first 2 bytes of this entry is the group index (in Big Endian
    order). This is followed by a #0-terminated group name. }

  SetLength(FGroupMap, GroupCount + 1);
  SetLength(IsNamed, GroupCount + 1);
  FillChar(IsNamed[0], (GroupCount + 1) * SizeOf(Integer), 255);

  { First, check which groups are named and which are not (that is, numbered) }
  P := NameTable;
  for Index := 0 to NameCount - 1 do
  begin
    GroupIndex := (PByte(P)^ shl 8) or (PByte(P + 1)^);
    Assert(GroupIndex >= 0);
    Assert(GroupIndex <= GroupCount);
    IsNamed[GroupIndex] := Index;
    Inc(P, NameEntrySize);
  end;

  { Add the numbered groups first... }
  TargetIndex := 0;
  for Index := 0 to GroupCount do
    if (IsNamed[Index] < 0) then
    begin
      FGroupMap[TargetIndex].Name := UTF8String(IntToStr(TargetIndex));
      FGroupMap[TargetIndex].CaptureIndex := Index;
      Inc(TargetIndex);
    end;

  { ...followed by the named groups. }
  for Index := 0 to GroupCount do
    if (IsNamed[Index] >= 0) then
    begin
      P := NameTable;
      Inc(P, IsNamed[Index] * NameEntrySize);
      Inc(P, 2);
      FGroupMap[TargetIndex].Name := UTF8String(P);
      FGroupMap[TargetIndex].CaptureIndex := Index;
      Inc(TargetIndex);
    end;
end;

procedure TRegex.Clear;
begin
  if (not FCached) then
    pcre_dispose(FCompiledPattern, FStudyData, nil);
  FCached := False;
  FCompiledPattern := nil;
  FStudyData := nil;
  FOffsetCount := 0;
  FGroupMap := nil;
end;

procedure TRegex.Compile;
var
  ErrorMessage: PAnsiChar;
  ErrorOffset: Integer;
begin
  Clear;
  FCompiledPattern := pcre_compile(PAnsiChar(FPattern), FInternalOptions,
    ErrorMessage, ErrorOffset, nil);
  if (FCompiledPattern = nil) then
    raise ERegexError.CreateFmt(RS_COMPILE_ERROR, [ErrorOffset, ErrorMessage]);
  CalculateGroupMap;
end;

class function TRegex.ConvertOptions(const Options: TRegexOptions): Integer;
begin
  Result := PCRE_UTF8 or PCRE_NEWLINE_ANY or PCRE_DUPNAMES;
  if (roIgnoreCase in Options) then
    Result := Result or PCRE_CASELESS;
  if (roMultiline in Options) then
    Result := Result or PCRE_MULTILINE;
  if (roExplicitCapture in Options) then
    Result := Result or PCRE_NO_AUTO_CAPTURE;
  if (roSingleline in Options) then
    Result := Result or PCRE_DOTALL;
  if (roIgnorePatternWhitespace in Options) then
    Result := Result or PCRE_EXTENDED;
end;

constructor TRegex.Create;
begin
  Create('', [], False);
end;

constructor TRegex.Create(const APattern: UTF8String);
begin
  Create(APattern, [], False);
end;

constructor TRegex.Create(const APattern: UTF8String;
  const AOptions: TRegexOptions);
begin
  Create(APattern, AOptions, False);
end;

constructor TRegex.Create(const APattern: UTF8String; const AOptions: TRegexOptions;
  const AUseCache: Boolean);
var
  CultureIndex: Integer;
  CultureKey, Key: UTF8String;
begin
  inherited Create;
  FPattern := APattern;
  FOptions := AOptions;
  FInternalOptions := ConvertOptions(AOptions);
  if (roSkipEmptyMatches in FOptions) then
    FExecFlags := PCRE_NOTEMPTY;

  CultureIndex := Languages.IndexOf(LANG_USER_DEFAULT);
  if (CultureIndex < 0) then
    CultureKey := ''
  else
    CultureKey := UTF8String(Languages.Ext[CultureIndex]);

  Key := UTF8String(IntToStr(Byte(FOptions))) + ':' + CultureKey + ':' + APattern;
  if (LookupCache(Key, FCompiledPattern, FStudyData)) then
  begin
    FCached := True;
    CalculateGroupMap;
  end
  else
  begin
    Compile;
    if (roStudy in AOptions) then
      Study;

    if (AUseCache) then
    begin
      AddToCache(Key, FCompiledPattern, FStudyData);
      FCached := True;
    end;
  end;
end;

destructor TRegex.Destroy;
begin
  Clear;
  FReplacementBuilder.Free;
  inherited;
end;

class function TRegex.Escape(const Str: UTF8String): UTF8String;
var
  I: Integer;
begin
  Result := Str;
  I := Length(Result);
  while (I > 0) do
  begin
    case Result[I] of
      #$09:
        begin
          Result[I] := 't';
          Insert('\', Result, I);
        end;
      #$0A:
        begin
          Result[I] := 'n';
          Insert('\', Result, I);
        end;
      #$0C:
        begin
          Result[I] := 'f';
          Insert('\', Result, I);
        end;
      #$0D:
        begin
          Result[I] := 'r';
          Insert('\', Result, I);
        end;
      ' ', '#', '$', '(', ')', '*', '+', '.', '?', '[', '\', '^', '{', '|':
        Insert('\', Result, I);
    end;
    Dec(I);
  end;
end;

function TRegex.ExpandSubstitutions(const Replacement: UTF8String): UTF8String;
(*PCRE does not support replacement patterns, so we expand the substitutions
  in the Replacement pattern ourselves.

  In .Net, the only special constructs allowed in replacements patterns are
  character escapes (starting with "\") and substitutions (starting with "$").
  Everything else is treated as regular text.

  The following substitutions are supported:
  $number: Substitutes the last substring matched by group number (decimal).
  ${name}: Substitutes the last substring matched by a (?<name>) group.
  $$     : Substitutes a single "$" literal.
  $&     : Substitutes a copy of the entire match itself.
  $`     : Substitutes all the text of the input string before the match.
  $'     : Substitutes all the text of the input string after the match.
  $+     : Substitutes the last group captured.
  $_     : Substitutes the entire input string. *)
var
  Start, I, J, Len, GroupNum: Integer;
  GroupName: UTF8String;
  C: AnsiChar;
begin
  if (FReplacementBuilder = nil) then
    FReplacementBuilder := TAnsiStringBuilder.Create;
  FReplacementBuilder.Clear;

  I := 1;
  Len := Length(Replacement);
  while (I <= Len) do
  begin
    C := Replacement[I];
    if (C = '$') and (I < Len) then
    begin
      { Substitution }
      Start := I;
      GroupNum := -1;
      Inc(I);
      case Replacement[I] of
        '$': { Substitutes a single "$" literal. }
          FReplacementBuilder.Append('$');

        '&': { Substitutes a copy of the entire match itself. }
          GroupNum := 0;

        '`': { Substitutes all the text of the input string before the match. }
          FReplacementBuilder.Append(FSubject, 1, FOffsets[0] - 1);

        '''': { Substitutes all the text of the input string after the match. }
          FReplacementBuilder.Append(FSubject, FOffsets[1], Length(FSubject) - FOffsets[1] + 1);

        '+': { Substitutes the last group captured. }
          GroupNum := FGroupMap[FOffsetCount - 1].CaptureIndex;

        '_': { Substitutes the entire input string. }
          FReplacementBuilder.Append(FSubject);

        '0'..'9': { Substitutes the last substring matched by group number. }
          begin
            { Parse the group number }
            GroupNum := Ord(Replacement[I]) - Ord('0');
            Inc(I);
            while (I <= Len) and (Replacement[I] in ['0'..'9']) do
            begin
              GroupNum := (GroupNum * 10) + (Ord(Replacement[I]) - Ord('0'));
              Inc(I);
            end;
            Dec(I);
            { Convert group number to capture index }
            if (GroupNum < Length(FGroupMap)) then
              GroupNum := FGroupMap[GroupNum].CaptureIndex
            else
              GroupNum := -2;
          end;

        '{': { Substitutes the last substring matched by a (?<name>) group. }
          begin
            Inc(I);
            if (I <= Len) then
            begin
              if (Replacement[I] in ['0'..'9']) then
              begin
                { The group name is a number. Parse the group number. }
                GroupNum := Ord(Replacement[I]) - Ord('0');
                Inc(I);
                while (I <= Len) and (Replacement[I] in ['0'..'9']) do
                begin
                  GroupNum := (GroupNum * 10) + (Ord(Replacement[I]) - Ord('0'));
                  Inc(I);
                end;
                { Convert group number to capture index }
                if (GroupNum < Length(FGroupMap)) then
                  GroupNum := FGroupMap[GroupNum].CaptureIndex
                else
                  GroupNum := -2;
              end
              else
              begin
                { Parse the group name }
                J := I;
                while (J <= Len) and (Replacement[J] <> '}') do
                  Inc(J);
                GroupName := Copy(Replacement, I, J - I);
                GroupNum := pcre_get_stringnumber(FCompiledPattern, PAnsiChar(GroupName));
                if (GroupNum < 0) then
                  GroupNum := -2;
                I := J;
              end;

              { Flag an error when the group doesn't end with a curly brace }
              if (I > Len) or (Replacement[I] <> '}') then
                GroupNum := -2;
            end;
          end
      else
        Dec(I);
      end;

      if (GroupNum >= 0) and (GroupNum < FOffsetCount) then
      begin
        { If we have successfully retrieved the group number, then add the
          replacement for that group. }
        if (GroupNum < FOffsetCount) then
          FReplacementBuilder.Append(Copy(FSubject, FOffsets[GroupNum * 2],
            FOffsets[GroupNum * 2 + 1] - FOffsets[GroupNum * 2]));
      end
      else if (GroupNum = -2) then
      begin
        { When there is an error in the replacement pattern, we insert the
          pattern as-is. }
        FReplacementBuilder.Append(C);
        I := Start;
      end;

      Inc(I);
    end
    else
    begin
      { Regular character }
      FReplacementBuilder.Append(C);
      Inc(I);
    end;
  end;

  Result := FReplacementBuilder.GetValue;
end;

function TRegex.GetGroupNames: TStringArray;
var
  I, J, K: Integer;
  Duplicate: Boolean;
begin
  Assert(Assigned(FGroupMap));
  SetLength(Result, Length(FGroupMap));
  K := 0;
  for I := 0 to Length(FGroupMap) - 1 do
  begin
    Duplicate := False;
    for J := 0 to I - 1 do
      if (FGroupMap[I].Name = FGroupMap[J].Name) then
      begin
        Duplicate := True;
        Break;
      end;

    if (not Duplicate) then
    begin
      Result[K] := FGroupMap[I].Name;
      Inc(K);
    end;
  end;
  SetLength(Result, K);
end;

function TRegex.GetGroupNumbers: TIntegerArray;
var
  I: Integer;
begin
  Assert(Assigned(FGroupMap));
  SetLength(Result, Length(FGroupMap));
  for I := 0 to Length(FGroupMap) - 1 do
    Result[I] := I;
end;

function TRegex.GetMatchLength: Integer;
begin
  Assert(FOffsetCount > 0);
  Result := FOffsets[1] - FOffsets[0];
end;

function TRegex.GetMatchIndex: Integer;
begin
  Assert(FOffsetCount > 0);
  Result := FOffsets[0];
end;

function TRegex.GetOptions: TRegexOptions;
begin
  Result := FOptions;
end;

function TRegex.GroupNameFromNumber(const I: Integer): UTF8String;
begin
  Assert(Assigned(FGroupMap));
  if (I >= Length(FGroupMap)) then
    raise EArgumentOutOfRangeException.Create(RS_ARGUMENT_OUT_OF_RANGE);
  Result := FGroupMap[I].Name;
end;

function TRegex.GroupNumberFromName(const Name: UTF8String): Integer;
begin
  Assert(Assigned(FGroupMap));
  for Result := 0 to Length(FGroupMap) - 1 do
    if (FGroupMap[Result].Name = Name) then
      Exit;
  Result := -1;
end;

class function TRegex.IsMatch(const Input, Pattern: UTF8String): Boolean;
var
  Regex: IRegex;
begin
  Regex := TRegex.Create(Pattern, [], True);
  Result := Regex.IsMatch(Input);
end;

class function TRegex.IsMatch(const Input, Pattern: UTF8String;
  const Options: TRegexOptions): Boolean;
var
  Regex: IRegex;
begin
  Regex := TRegex.Create(Pattern, Options, True);
  Result := Regex.IsMatch(Input);
end;

function TRegex.IsMatch(const Input: UTF8String): Boolean;
begin
  SetSubject(Input);
  Result := Match;
end;

function TRegex.IsMatch(const Input: UTF8String; const StartAt: Integer): Boolean;
begin
  SetSubject(Input);
  FStartOffset := StartAt;
  Result := Match;
end;

procedure TRegex.Load(const Stream: TStream);
var
  Header: TRegexHeader;
begin
  Clear;
  Stream.ReadBuffer(Header, SizeOf(Header));
  if (Header.Signature <> REGEX_SIGNATURE) then
    raise ERegexError.Create(RS_INVALID_SIGNATURE);
  if (Header.Version > REGEX_VERSION) then
    raise ERegexError.Create(RS_UNSUPPORTED_VERSION);

  if (Header.CompiledSize > 0) then
  begin
    GetMem(FCompiledPattern, Header.CompiledSize);
    Stream.ReadBuffer(FCompiledPattern^, Header.CompiledSize);
    if (Header.StudyDataSize > 0) then
    begin
      GetMem(FStudyData, Header.StudyDataSize);
      Stream.ReadBuffer(FStudyData^, Header.StudyDataSize);
    end;
  end;

  CalculateGroupMap;
end;

class function TRegex.LookupCache(const Key: UTF8String; out CompiledPattern,
  StudyData: Pointer): Boolean;
var
  I: Integer;
  Entry: TCachedCompiledPattern;
begin
  for I := 0 to Length(FCache) - 1 do
  begin
    Entry := FCache[I];
    if Assigned(Entry) and (Entry.FKey = Key) then
    begin
      Result := True;
      CompiledPattern := Entry.FCompiledPattern;
      StudyData := Entry.FStudyData;
      if (I > 0) then
      begin
        Move(FCache[0], FCache[1], I * SizeOf(TCachedCompiledPattern));
        FCache[0] := Entry;
      end;
      Exit;
    end;
  end;
  Result := False;
  CompiledPattern := nil;
  StudyData := nil;
end;

function TRegex.Match(const Input: UTF8String; const Beginning,
  Length: Integer): IMatch;
begin
  SetSubject(Input);
  FStartOffset := Beginning;
  FMaxLength := Length;
  if Match then
    Result := TMatch.Create(Self, FSubject, GetMatchIndex, GetMatchLength)
  else
    Result := TMatch.Empty;
end;

class function TRegex.Match(const Input, Pattern: UTF8String): IMatch;
var
  Regex: IRegex;
begin
  Regex := TRegex.Create(Pattern, [], True);
  Result := Regex.Match(Input);
end;

class function TRegex.Match(const Input, Pattern: UTF8String; StartAt: Integer): IMatch;
var
  Regex: IRegex;
begin
  Regex := TRegex.Create(Pattern, [], True);
  Result := Regex.Match(Input, StartAt);
end;

class function TRegex.Match(const Input, Pattern: UTF8String;
  const Options: TRegexOptions): IMatch;
var
  Regex: IRegex;
begin
  Regex := TRegex.Create(Pattern, Options, True);
  Result := Regex.Match(Input);
end;

class function TRegex.Match(const Input, Pattern: UTF8String;
  const Options: TRegexOptions; StartAt, Count: Integer): IMatch;
var
  Regex: IRegex;
begin
  Regex := TRegex.Create(Pattern, Options, True);
  Result := Regex.Match(Input, StartAt, Count);
end;

function TRegex.Match: Boolean;
var
  I: Integer;
begin
  Assert(Assigned(FCompiledPattern));
  FOffsetCount := pcre_exec(FCompiledPattern, FStudyData, PAnsiChar(FSubject),
    FMaxLength, FStartOffset - 1, FExecFlags, @FOffsets[0], Length(FOffsets));
  Result := (FOffsetCount > 0);
  if Result then begin
    for I := 0 to (FOffsetCount * 2) - 1 do
      Inc(FOffsets[I]);
    FStartOffset := FOffsets[1];
    if (FOffsets[0] = FOffsets[1]) then
      if (FStartOffset <= Length(FSubject)) then
        Inc(FStartOffset, UTF8ByteCount(FSubject[FStartOffset]))
      else
        Inc(FStartOffset);
  end;
end;

function TRegex.Match(const Input: UTF8String): IMatch;
begin
  SetSubject(Input);
  if (Match) then
    Result := TMatch.Create(Self, FSubject, GetMatchIndex, GetMatchLength)
  else
    Result := TMatch.Empty;
end;

function TRegex.Match(const Input: UTF8String; const StartAt: Integer): IMatch;
begin
  SetSubject(Input);
  FStartOffset := StartAt;
  if (Match) then
    Result := TMatch.Create(Self, FSubject, GetMatchIndex, GetMatchLength)
  else
    Result := TMatch.Empty;
end;

function TRegex.Matches(const Input: UTF8String;
const StartAt: Integer): IMatchCollection;
var
  Collection: TMatchCollection;
begin
  Collection := TMatchCollection.Create;
  Result := Collection;
  SetSubject(Input);
  FStartOffset := StartAt;
  if Match then
  repeat
    Collection.Add(TMatch.Create(Self, FSubject, GetMatchIndex, GetMatchLength));
  until (not MatchNext);
end;

function TRegex.Matches(const Input: UTF8String): IMatchCollection;
begin
  Result := Matches(Input, 1);
end;

class function TRegex.Matches(const Input, Pattern: UTF8String;
  const Options: TRegexOptions): IMatchCollection;
var
  Regex: IRegex;
begin
  Regex := TRegex.Create(Pattern, Options, True);
  Result := Regex.Matches(Input);
end;

class function TRegex.Matches(const Input, Pattern: UTF8String;
  const Options: TRegexOptions; StartAt: Integer): IMatchCollection;
var
  Regex: IRegex;
begin
  Regex := TRegex.Create(Pattern, Options, True);
  Result := Regex.Matches(Input, StartAt);
end;

class function TRegex.Matches(const Input, Pattern: UTF8String): IMatchCollection;
var
  Regex: IRegex;
begin
  Regex := TRegex.Create(Pattern, [], True);
  Result := Regex.Matches(Input);
end;

class function TRegex.Matches(const Input, Pattern: UTF8String;
  StartAt: Integer): IMatchCollection;
var
  Regex: IRegex;
begin
  Regex := TRegex.Create(Pattern, [], True);
  Result := Regex.Matches(Input, StartAt);
end;

function TRegex.MatchNext: Boolean;
var
  I: Integer;
begin
  Assert(Assigned(FCompiledPattern));
  if ((FStartOffset - 1) > FMaxLength) then
    FOffsetCount := -1
  else
    FOffsetCount := pcre_exec(FCompiledPattern, FStudyData, PAnsiChar(FSubject),
      FMaxLength, FStartOffset - 1, FExecFlags, @FOffsets[0], Length(FOffsets));

  Result := (FOffsetCount > 0);

  if Result then begin
    for I := 0 to (FOffsetCount * 2) - 1 do
      Inc(FOffsets[I]);
    FStartOffset := FOffsets[1];
    if (FOffsets[0] = FOffsets[1]) then
      if (FStartOffset <= Length(FSubject)) then
        Inc(FStartOffset, UTF8ByteCount(FSubject[FStartOffset]))
      else
        Inc(FStartOffset);
  end;
end;

class function TRegex.Replace(const Input, Pattern: UTF8String;
  const Evaluator: TMatchEvaluator): UTF8String;
var
  Regex: IRegex;
begin
  Regex := TRegex.Create(Pattern, [], True);
  Result := Regex.Replace(Input, Evaluator);
end;

class function TRegex.Replace(const Input, Pattern, Replacement: UTF8String;
  const Options: TRegexOptions): UTF8String;
var
  Regex: IRegex;
begin
  Regex := TRegex.Create(Pattern, Options, True);
  Result := Regex.Replace(Input, Replacement)
end;

function TRegex.Replace(const Input, Replacement: UTF8String;
  const Count: Integer): UTF8String;
begin
  Result := Replace(Input, Replacement, Count, 1);
end;

function TRegex.Replace(const Input: UTF8String;
  const Evaluator: TMatchEvaluator): UTF8String;
begin
  FEvaluator := Evaluator;
  try
    Result := Replace(Input, '');
  finally
    FEvaluator := nil;
  end;
end;

function TRegex.Replace(const Input, Replacement: UTF8String): UTF8String;
begin
  Result := Replace(Input, Replacement, -1);
end;

function TRegex.Replace(const Input: UTF8String; const Evaluator: TMatchEvaluator;
  const Count: Integer): UTF8String;
begin
  FEvaluator := Evaluator;
  try
    Result := Replace(Input, '', Count);
  finally
    FEvaluator := nil;
  end;
end;

class function TRegex.Replace(const Input, Pattern: UTF8String;
  const Evaluator: TMatchEvaluator; const Options: TRegexOptions): UTF8String;
var
  Regex: IRegex;
begin
  Regex := TRegex.Create(Pattern, Options, True);
  Result := Regex.Replace(Input, Evaluator);
end;

function TRegex.Replace: UTF8String;
var
  Offset, Len, Delta: Integer;
  Match: IMatch;
begin
  Result := ExpandSubstitutions(FReplacement);
  Offset := GetMatchIndex;
  Len := GetMatchLength;

  if Assigned(FEvaluator) then
  begin
    Match := TMatch.Create(Self, FSubject, Offset, Len);
    Result := FEvaluator(Match);
  end;

  Delete(FSubject, Offset, Len);
  if (Result <> '') then
    Insert(Result, FSubject, Offset);
  Delta := Length(Result) - Len;
  Inc(FStartOffset, Delta);
  Inc(FMaxLength, Delta);
  FOffsetCount := 0;
end;

class function TRegex.Replace(const Input, Pattern,
  Replacement: UTF8String): UTF8String;
var
  Regex: IRegex;
begin
  Regex := TRegex.Create(Pattern, [], True);
  Result := Regex.Replace(Input, Replacement)
end;

function TRegex.Replace(const Input, Replacement: UTF8String; const Count,
  StartAt: Integer): UTF8String;
var
  MatchCount: Integer;
begin
  if (Count = 0) then
    Exit(Input);

  if (Count < 0) then
    MatchCount := MaxInt
  else
    MatchCount := Count;

  SetSubject(Input);
  FStartOffset := StartAt;
  FReplacement := Replacement;

  if Match then
  repeat
    Replace;
    Dec(MatchCount);
  until (MatchCount = 0) or (not MatchNext);

  Result := FSubject;
end;

function TRegex.Replace(const Input: UTF8String; const Evaluator: TMatchEvaluator;
  const Count, StartAt: Integer): UTF8String;
begin
  FEvaluator := Evaluator;
  try
    Result := Replace(Input, '', Count, StartAt);
  finally
    FEvaluator := nil;
  end;
end;

procedure TRegex.Save(const Stream: TStream);
var
  Header: TRegexHeader;
begin
  Assert(Assigned(FCompiledPattern));
  Header.Signature := REGEX_SIGNATURE;
  Header.Version := REGEX_VERSION;
  Header.CompiledSize := 0;
  Header.StudyDataSize := 0;

  if (pcre_fullinfo(FCompiledPattern, nil, PCRE_INFO_SIZE, @Header.CompiledSize) >= 0) then
  begin
    if Assigned(FStudyData) then
      pcre_fullinfo(FCompiledPattern, FStudyData, PCRE_INFO_STUDYSIZE, @Header.StudyDataSize)
  end
  else
    Header.CompiledSize := 0;
  Stream.WriteBuffer(Header, SizeOf(Header));

  if (Header.CompiledSize >= 0) then
  begin
    Stream.WriteBuffer(FCompiledPattern^, Header.CompiledSize);
    if Assigned(FStudyData) then
      Stream.WriteBuffer(FStudyData^, Header.StudyDataSize);
  end;
end;

class procedure TRegex.SetCacheSize(const Value: Integer);
var
  I: Integer;
begin
  if (Value < 0) then
    raise EArgumentOutOfRangeException.Create(RS_ARGUMENT_OUT_OF_RANGE);
  if (Value <> FCacheSize) then
  begin
    for I := Value to FCacheSize - 1 do
      FCache[I].Free;
    SetLength(FCache, Value);
    for I := FCacheSize to Value - 1 do
      FCache[I] := nil;
    FCacheSize := Value;
  end;
end;

procedure TRegex.SetSubject(const Value: UTF8String);
begin
  FSubject := Value;
  FStartOffset := 1;
  FMaxLength := Length(FSubject);
  FOffsetCount := 0;
end;

class function TRegex.Split(const Input, Pattern: UTF8String;
  const Options: TRegexOptions): TStringArray;
var
  Regex: TRegex;
begin
  Regex := TRegex.Create(Pattern, Options, True);
  Result := Regex.Split(Input);
end;

class function TRegex.Split(const Input, Pattern: UTF8String): TStringArray;
var
  Regex: TRegex;
begin
  Regex := TRegex.Create(Pattern, [], True);
  Result := Regex.Split(Input);
end;

function TRegex.Split(const Input: UTF8String; const Count,
  StartAt: Integer): TStringArray;
var
  Cnt: Integer;
  Match: IMatch;
  SL: TStringList;
  I, PrevAt: Integer;
  Group: IGroup;
begin
  if (Count < 0) then
    raise EArgumentOutOfRangeException.Create(RS_ARGUMENT_OUT_OF_RANGE);

  if (StartAt < 1) or (StartAt > Length(Input) + 1) then
    raise EArgumentOutOfRangeException.Create(RS_ARGUMENT_OUT_OF_RANGE);

  if (Count = 1) then
  begin
    SetLength(Result, 1);
    Result[0] := Input;
    Exit;
  end;

  Cnt := Count - 1;
  Match := Self.Match(Input, StartAt);
  if (not Match.Success) then
  begin
    SetLength(Result, 1);
    Result[0] := Input;
    Exit;
  end;

  SL := TStringList.Create;
  try
    PrevAt := 1;
    while True do
    begin
      SL.Add(Copy(Input, PrevAt, Match.Index - PrevAt));
      PrevAt := Match.Index + Match.Length;

      for I := 1 to Match.Groups.Count - 1 do
      begin
        Group := Match.Groups[I];
        if (Group.Success) then
          SL.Add(Group.ToString);
      end;

      Dec(Cnt);
      if (Cnt = 0) then
        Break;

      Match := Match.NextMatch;
      if (not Match.Success) then
        Break;
    end;
    SL.Add(Copy(Input, PrevAt, Length(Input) - PrevAt + 1));

    SetLength(Result, SL.Count);
    for I := 0 to SL.Count - 1 do
      Result[I] := SL[I];
  finally
    SL.Free;
  end;
end;

function TRegex.Split(const Input: UTF8String): TStringArray;
begin
  Result := Split(Input, 0, 1);
end;

function TRegex.Split(const Input: UTF8String;
  const Count: Integer): TStringArray;
begin
  Result := Split(Input, Count, 1);
end;

class procedure TRegex.StaticFinalize;
var
  I: Integer;
begin
  for I := 0 to Length(FCache) - 1 do
    FCache[I].Free;
end;

class procedure TRegex.StaticInitialize;
begin
  FCacheSize := 15;
  SetLength(FCache, FCacheSize);
  FillChar(FCache[0], FCacheSize * SizeOf(TCachedCompiledPattern), 0);
end;

procedure TRegex.Study;
var
  Error: PAnsiChar;
begin
  Assert(Assigned(FCompiledPattern));
  FStudyData := pcre_study(FCompiledPattern, 0, Error);
  if Assigned(Error) then
    raise ERegexError.CreateFmt(RS_STUDY_ERROR, [Error]);
end;

function TRegex.ToString: String;
begin
  Result := UTF8ToUnicodeString(FPattern);
end;

class function TRegex.Unescape(const Str: UTF8String): UTF8String;
var
  I, J, K, C, D, E: Integer;
begin
  Result := Str;
  I := 1;
  while (I < Length(Result)) do
  begin
    if (Result[I] = '\') then
    begin
      Delete(Result, I, 1);
      case Result[I] of
        '0'..'7':
          begin
            { Convert octal escape code }
            C := 3;
            J := 0;
            while (I <= Length(Result)) and (C > 0) do
            begin
              D := Ord(Result[I]) - Ord('0');
              if (D < 0) or (D > 7) then
                Break;
              J := (J * 8) + D;
              Dec(C);
              Inc(I);
            end;
            Dec(I, (3 - C));
            if (C < 2) then
              Delete(Result, I, 2 - C);
            Result[I] := AnsiChar(J and $FF);
          end;
        'x', 'u':
          begin
            { Convert hex escape code }
            K := I;
            if (Result[I] = 'x') then
              C := 2
            else
              C := 4;
            E := C;
            Inc(I);
            J := 0;
            while (I <= Length(Result)) and (C > 0) do
            begin
              case Result[I] of
                '0'..'9':
                  D := Ord(Result[I]) - Ord('0');
                'a'..'z':
                  D := Ord(Result[I]) - Ord('a') + 10;
                'A'..'Z':
                  D := Ord(Result[I]) - Ord('A') + 10;
              else
                D := -1;
              end;
              if (D < 0) then
                Break;
              J := (J * 16) + D;
              Dec(C);
              Inc(I);
            end;
            if (C > 0) then
              raise EArgumentException.Create(RS_TOO_FEW_HEX_CHARS);
            Delete(Result, K, E);
            Result[K] := AnsiChar(J);
            I := K;
          end;
        'a':
          Result[I] := #$07;
        'b':
          Result[I] := #$08;
        'e':
          Result[I] := #$1B;
        'f':
          Result[I] := #$0C;
        'n':
          Result[I] := #$0A;
        'r':
          Result[I] := #$0D;
        't':
          Result[I] := #$09;
        'v':
          Result[I] := #$0B;
        'c':
          begin
            Delete(Result, I, 1);
            if (I > Length(Result)) then
              raise EArgumentException.Create(RS_MISSING_CONTROL);
            C := Ord(Result[I]);
            if (C >= Ord('a')) and (C <= Ord('z')) then
              C := C - (Ord('a') - Ord('A'));
            C := C - Ord('@');
            if (C < Ord(' ')) then
              Result[I] := AnsiChar(C)
            else
              raise EArgumentException.Create(RS_UNRECOGNIZED_CONTROL);
          end;
      end;
    end;

    Inc(I);
  end;
end;

{ TRegex.TCachedCompiledPattern }

constructor TRegex.TCachedCompiledPattern.Create(const AKey: UTF8String;
  const ACompiledPattern, AStudyData: Pointer);
begin
  inherited Create;
  FKey := AKey;
  FCompiledPattern := ACompiledPattern;
  FStudyData := AStudyData;
end;

destructor TRegex.TCachedCompiledPattern.Destroy;
begin
  pcre_dispose(FCompiledPattern, FStudyData, nil);
  inherited;
end;

{ TRegex.TAnsiStringBuilder }

procedure TRegex.TAnsiStringBuilder.Append(const Value: AnsiChar);
begin
  Append(@Value, 1);
end;

procedure TRegex.TAnsiStringBuilder.Append(const Buffer: Pointer;
  const Size: Integer);
begin
  if (Size > 0) then
  begin
    if ((FLength + Size) > FCapacity) then
      Grow(Size);
    Move(Buffer^, FCurrent^, Size);
    Inc(FCurrent, Size);
    Inc(FLength, Size);
  end;
end;

procedure TRegex.TAnsiStringBuilder.Append(const Value: UTF8String;
  const Index, Count: Integer);
begin
  if (Value <> '') then
    Append(@Value[Index], Count);
end;

procedure TRegex.TAnsiStringBuilder.Append(const Value: UTF8String);
begin
  if (Value <> '') then
    Append(@Value[1], Length(Value));
end;

procedure TRegex.TAnsiStringBuilder.Clear;
begin
  FLength := 0;
  FCurrent := FBuffer;
end;

constructor TRegex.TAnsiStringBuilder.Create;
begin
  inherited Create;
  FCapacity := 256;
  GetMem(FBuffer, FCapacity);
  FCurrent := FBuffer;
end;

destructor TRegex.TAnsiStringBuilder.Destroy;
begin
  FreeMem(FBuffer);
  inherited;
end;

function TRegex.TAnsiStringBuilder.GetValue: UTF8String;
begin
  if (FBuffer = nil) then
    Result := ''
  else
    SetString(Result, FBuffer, FLength);
end;

procedure TRegex.TAnsiStringBuilder.Grow(const Size: Integer);
var
  BytesNeeded: Integer;
begin
  BytesNeeded := FLength + Size - FCapacity;
  BytesNeeded := ((BytesNeeded + 255) div 256) * 256;
  Inc(FCapacity, BytesNeeded);
  ReallocMem(FBuffer, FCapacity);
  FCurrent := FBuffer;
  Inc(FCurrent, FLength);
end;

{ TCollection<T> }

procedure TCollection<T>.Add(const Value: T);
begin
  if (FCount = FCapacity) then
    Grow;
  FItems[FCount] := Value;
  Inc(FCount);
end;

function TCollection<T>.GetCount: Integer;
begin
  Result := FCount;
end;

function TCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TCollection<T>.GetIsReadOnly: Boolean;
begin
  Result := True;
end;

function TCollection<T>.GetIsSynchronized: Boolean;
begin
  Result := False;
end;

function TCollection<T>.GetItem(const Index: Integer): T;
begin
  if (Index < 0) or (Index >= FCount) then
    raise EArgumentOutOfRangeException.Create(RS_ARGUMENT_OUT_OF_RANGE);
  Result := FItems[Index];
end;

procedure TCollection<T>.Grow;
var
  Delta: Integer;
begin
  if (FCapacity > 64) then
    Delta := FCapacity div 4
  else
  if (FCapacity > 8) then
    Delta := 16
  else
    Delta := 4;
  SetLength(FItems, FCapacity + Delta);
  FillChar(FItems[FCapacity], Delta * SizeOf(T), 0);
  Inc(FCapacity, Delta);
end;

{ TCollection<T>.TEnumerator }

constructor TCollection<T>.TEnumerator.Create(const ACollection: TCollection<T>);
begin
  inherited Create;
  FCollection := ACollection;
  FIndex := -1;
end;

function TCollection<T>.TEnumerator.GetCurrent: T;
begin
  Result := FCollection.FItems[FIndex];
end;

function TCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := (FIndex < FCollection.FCount - 1);
  if Result then
    Inc(FIndex);
end;

{ TCapture }

constructor TCapture.Create(const AValue: UTF8String; const AIndex,
  ALength: Integer);
begin
  inherited Create;
  FValue := AValue;
  FIndex := AIndex;
  FLength := ALength
end;

function TCapture.GetIndex: Integer;
begin
  Result := FIndex;
end;

function TCapture.GetLength: Integer;
begin
  Result := FLength;
end;

function TCapture.GetValue: UTF8String;
begin
  if (FIndex <= 0) then
    Result := ''
  else
    Result := Copy(FValue, FIndex, FLength);
end;

function TCapture.ToString: String;
begin
  Result := UTF8ToUnicodeString(GetValue);
end;

{ TGroup }

class function TGroup.GetEmptyGroup: IGroup;
begin
  if (FEmptyGroup = nil) then
    FEmptyGroup := TGroup.Create('', 0, 0);
  Result := FEmptyGroup;
end;

function TGroup.GetSuccess: Boolean;
begin
  Result := (FIndex > 0);
end;

{ TGroupCollection }

constructor TGroupCollection.Create(const AMatch: TMatch);
begin
  inherited Create;
  FMatch := AMatch;
end;

function TGroupCollection.GetItemByName(const GroupName: UTF8String): IGroup;
var
  GroupNum, Count: Integer;
begin
  if (FMatch = nil) or (FMatch.FRegex = nil) then
    Exit(TGroup.EmptyGroup);

  Result := nil;
  Count := Length(FMatch.FRegex.FGroupMap);
  if (Count > FCount) then
    Count := FCount;
  for GroupNum := 0 to Count - 1 do
    if (FMatch.FRegex.FGroupMap[GroupNum].Name = GroupName) then
    begin
      Result := FItems[GroupNum];
      if (Result.Success) then
        Exit;
    end;

  if (Result = nil) then
    Result := TGroup.EmptyGroup;
end;

function TGroupCollection.GetSyncRoot: TObject;
begin
  Result := FMatch;
end;

{ TMatch }

constructor TMatch.Create(const ARegex: TRegex; const AValue: UTF8String;
  const AIndex, ALength: Integer);
begin
  inherited Create(AValue, AIndex, ALength);
  FRegex := ARegex;
  FRegexIntf := ARegex;
end;

class function TMatch.GetEmpty: IMatch;
begin
  if (FEmpty = nil) then
    FEmpty := TMatch.Create(nil, '', 0, 0);
  Result := FEmpty;
end;

function TMatch.GetGroups: IGroupCollection;
var
  Groups: TGroupCollection;
  I, CaptureIndex: Integer;
begin
  if (FGroups = nil) then
  begin
    Groups := TGroupCollection.Create(Self);
    FGroups := Groups;
    if (FIndex > 0) then
    begin
      Assert(Assigned(FRegex.FGroupMap));
      Assert(FRegex.FOffsetCount <= Length(FRegex.FGroupMap));
      for I := 0 to FRegex.FOffsetCount - 1 do
      begin
        CaptureIndex := FRegex.FGroupMap[I].CaptureIndex;
        Groups.Add(TGroup.Create(FValue, FRegex.FOffsets[CaptureIndex * 2],
          FRegex.FOffsets[CaptureIndex * 2 + 1] - FRegex.FOffsets[CaptureIndex * 2]));
      end;
    end
    else
      Groups.Add(TGroup.Create('', 0, 0));
  end;
  Result := FGroups;
end;

function TMatch.NextMatch: IMatch;
begin
  if Assigned(FRegex) and (FRegex.MatchNext) then
    Result := TMatch.Create(FRegex, FValue, FRegex.GetMatchIndex,
      FRegex.GetMatchLength)
  else
    Result := TMatch.Empty;
end;

function TMatch.Result(const Replacement: UTF8String): UTF8String;
begin
  if (FRegex = nil) then
    raise EArgumentException.Create(RS_ARGUMENT_NIL);
  Result := FRegex.ExpandSubstitutions(Replacement);
end;

{ TMatchCollection }

function TMatchCollection.GetSyncRoot: TObject;
begin
  Result := Self;
end;

initialization
  TRegex.StaticInitialize;

finalization
  TRegex.StaticFinalize;

end.
