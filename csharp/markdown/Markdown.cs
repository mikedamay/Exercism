using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;


internal enum TokenType /* or stateType */ { Root, Para, Strong, Italic, Text, Newline
    ,Invalid
    ,End
    ,Heading1Start, Heading1, Heading2Start, Heading2, Heading3Start, Heading3
    ,Heading4Start, Heading4, Heading5Start, Heading5, Heading6Start, Heading6
    , List, ListItemStart, ListItem, ListContinuation
    ,SpaceOrTab
    ,
    EnhancedTextEnding,
    EnhancedTextStarting
}

/// <summary>
/// This follows XML type parsing where each bit of text between markup is considered a separate child node/token
/// The markdown rules are more or less based on Jetbrains' Markdown plugin rules, ie. except where
/// the author missed the point or disagreed with the approach
///
/// TODO: sort out the fragmentStart index for non-text tokens
/// </summary>
public static class Markdown
{
    public static string Parse(string markdown)
    {
        if (markdown == null)
        {
            return string.Empty;
        }
        IEnumerable<Token> tokens = new Analyser(markdown.ToCharArray()).Tokens;

        var sb = new StringBuilder();
        DoParse(tokens, sb);
        return sb.ToString();
    }

    private static void DoParse(IEnumerable<Token> tokens, StringBuilder sb)
    {
        if (tokens == null)
        {
            return;
        }

        foreach (Token token in tokens)
        {
            switch (token.Type)
            {
                case TokenType.End:
                    break;
                case TokenType.Newline:
                    break;
                default:
                    sb.Append(token.BeforeTag);    // in additon to normal tokens with their html tags
                                                   // the text token will return its text here
                    DoParse(token.Children, sb);
                    sb.Append(token.AfterTag);      // text token just returns an empty string here
                    break;
            }
        }
    }
}

internal class Analyser
{

    public IEnumerable<Token> Tokens { get; }
    
    private enum  StackDataType {Data, Token}
    
    private int idx;
    private readonly char[] chars;
    private int fragmentStart = 0;
    
    private const char HEADING_CHAR = '#';
    private const char LIST_CHAR = '*';
    private const char UNDERSCORE_CHAR = '_';
    private const char TEXT_CHAR = 'A';
    private const char NEWLINE_CHAR = '\n';
    private const char SPACE_OR_TAB_CHAR = ' ';
    private const char END_CHAR = '\0';
    private const char OTHER_CHAR = (char)1;
    
    /// <summary>
    /// an instance of TokenData is pushed onto the tokenStack when a character
    /// is encountered which will result in a new token.
    /// </summary>
    private struct TokenData
    {
        public TokenType TokenType { get; }
        public int FragmentStart { get; }
        public TokenData(TokenType tokenType, int fragmentStart)
        {
            TokenType = tokenType;
            FragmentStart = fragmentStart;
        }
    }

    /// <summary>
    /// an instance of tokenStack data (containing an instance of token data) is pushed
    /// onto the tokenStack when a character is encountered which will give rise to a new token.
    /// When that token is eventually complete the instance of tokenStack data containing the
    /// token data is popped off and an instance containing an actual token is pushed
    /// </summary>
    private class StackData
    {
        private readonly Token token;
        private readonly TokenData? tokenData;

        public Token Token
            => token ?? throw new Exception("tokenStack contains token data - complete token not available");
        public TokenData TokenData
            => tokenData ?? throw new Exception("tokenStack contains complete token - token data not available");

        public StackDataType DataType => token != null ? StackDataType.Token : StackDataType.Data;

        public StackData(Token token)
        {
            this.token = token;
        }

        public StackData(TokenData tokenData)
        {
            this.tokenData = tokenData;
        }
    }
    
    private Stack<StackData> tokenStack = new Stack<StackData>();

    private Stack<IReadOnlyDictionary<(TokenType, char)
      , (TokenType, Action<Analyser, TokenType>, Action<Analyser, TokenType>)>> transitionStack
        = new Stack<IReadOnlyDictionary<(TokenType, char)
          , (TokenType, Action<Analyser, TokenType>, Action<Analyser, TokenType>)>>();

    public Analyser(char[] chars)
    {
        this.chars = chars;
        transitionStack.Push(transitions);
        Tokens = Analyse();
    }
    
    /// <summary>
    /// uses: chars and transitions
    /// modifies: idx and startFragmentIdx
    /// produces: tokenStack (as a list)
    /// </summary>
    private IEnumerable<Token> Analyse()
    {
        var state = TokenType.Root;
        Debug.WriteLine("started:");
        while (true)
        {
            char ch = Next();
            
            string actualChar = ch == '\n' ? "\\n" : new string(new char[] {idx < chars.Length ? chars[idx] : '$'});
            string effectiveChar  = ch == '\n' ? "\\n"  : ch == '\0' ? "$" : new string(new char[] {ch});
            Debug.Write($"state == {state,20} char == '{actualChar, 2}' / '{effectiveChar, 2}' idx == {idx,3} start == {fragmentStart, 3}");
            
            Action<Analyser, TokenType> actionBefore;
            Action<Analyser, TokenType> actionAfter;
            TokenType newState;
            (newState, actionBefore, actionAfter) = GetTransition(state, ch);
            actionBefore(this, state);
            if (state == TokenType.End)
            {
                Debug.WriteLine(string.Empty);
                break;
            }
            idx++;
            actionAfter(this, newState);
            state = newState;
            
            Debug.WriteLine($" newState == {state} idx == {idx} start == {fragmentStart,3}");

        }

        return tokenStack.Reverse().Select(sd => sd.Token).ToList();
    }

    private char Next()
    {
        char ch;
        if (idx >= chars.Length)
        {
            return END_CHAR;
        }

        switch (chars[idx])
        {
            case HEADING_CHAR:
            case LIST_CHAR:
            case NEWLINE_CHAR:
            case UNDERSCORE_CHAR:
                ch = chars[idx];
                break;
            case ' ':
            case '\t':
                ch = SPACE_OR_TAB_CHAR;
                break;
            default:
                ch = TEXT_CHAR;
                break;
        }

        return ch;
    }

    private void Unread(int num)
    {
        idx -= num;
    }
    private (TokenType, Action<Analyser, TokenType>, Action<Analyser, TokenType>) GetTransition(TokenType state, char ch)
    {
        var trans = transitionStack.Pop();
        try
        {
            if (trans.ContainsKey((state, ch)))
            {
                return trans[(state, ch)];
            }
            else if (transitionStack.Count > 0)
            {
                return GetTransition(state, ch);
            }
            else
            {
                return trans[(state, OTHER_CHAR)];
            }
        }
        catch (Exception inner)
        {
            var ex = new Exception($"no transition available for {state} and {ch}", inner);
            Debug.WriteLine(ex);
            throw ex;
        }
        finally
        {
            transitionStack.Push(trans);
        }
    }

    /// <summary>
    /// </summary>
    /// <param name="this_">the (one and only)  analyser instance which is doing all this</param>
    /// <param name="tokenType">The current state of the machine</param>
    private static void Noop(Analyser this_, TokenType tokenType = TokenType.Invalid)
    {
    }
    private static void DecrementIdx(Analyser this_, TokenType tokenType = TokenType.Invalid)
    {
        this_.Unread(1);
    }
    private static void Error(Analyser this_, TokenType tokenType = TokenType.Invalid)
    {
        throw new Exception($"markdown analysis failed at position {this_.idx} processing {tokenType}");
    }

    private static void PushTokenDataPlus(Analyser this_, TokenType tokenType)
    {
        PushTokenData(this_, tokenType);
        Bookmark(this_, tokenType);
     }
    private static void PushTokenData(Analyser this_, TokenType tokenType)
    {
        MyDebug.Assert(
            tokenType == TokenType.Heading1
            || tokenType == TokenType.Heading2
            || tokenType == TokenType.Heading3
            || tokenType == TokenType.Heading4
            || tokenType == TokenType.Heading5
            || tokenType == TokenType.Heading6
            || tokenType == TokenType.Para
            || tokenType == TokenType.List
            || tokenType == TokenType.ListItem
            || tokenType == TokenType.Italic
            || tokenType == TokenType.Strong
            );
        this_.tokenStack.Push(new StackData(new TokenData(tokenType, this_.GetBookmarkIdx())));
    }

    /// <summary>
    /// tokenStack might look something like:
    /// 
    /// Token: Heading
    /// Token: Para
    /// Token: List
    /// Token: Para
    /// TokenData: Para
    /// Token: Text
    /// Token Italic
    /// Token: Text 
    /// Newline or End (which will convert Strong to a token, Para to a token) and which will leave
    /// 
    /// Token: Heading
    /// Token: Para
    /// Token: List
    /// Token: Para
    /// Token: Para
    ///
    /// Final results will be left as a set of tokenStack frames that can be converted to a list
    /// </summary>
    /// <param name="this_">the main (one and only) analyser</param>
    /// <param name="tokenType">state such as Newline, End, ItalicEnd or StrongEnd</param>
    private static void PushToken(Analyser this_, TokenType tokenType)
    {
        MyDebug.Assert(tokenType == TokenType.End
          || tokenType == TokenType.Newline
          || tokenType == TokenType.ListContinuation
                        );

        List<Token> tokens = new List<Token>();
        do
        {
            if (this_.tokenStack.Count == 0)
                break;
            var stackData = this_.tokenStack.Pop();
            if (AreMatchingTokenTypes(this_, tokenType, stackData))
            {
                var newToken = new Token(stackData.TokenData.TokenType, new List<Token>(tokens));
                this_.tokenStack.Push(new StackData(newToken));
                break;
            }
            else if (stackData.DataType == StackDataType.Data)
            {
                var newToken = new Token(stackData.TokenData.TokenType, new List<Token>(tokens));
                tokens.Clear();
                tokens.Insert(0, newToken);
            }
            else
            {
                tokens.Insert(0, stackData.Token);
            }
        } while (true);
        Bookmark(this_, tokenType);
    }

    private static ISet<(TokenType, TokenType)> matchingTokens = new HashSet<(TokenType, TokenType)>
    {
        (TokenType.End, TokenType.Heading1),
        (TokenType.End, TokenType.Heading2),
        (TokenType.End, TokenType.Heading3),
        (TokenType.End, TokenType.Heading4),
        (TokenType.End, TokenType.Heading5),
        (TokenType.End, TokenType.Heading6),
        (TokenType.End, TokenType.Para),
        (TokenType.End, TokenType.List),
        (TokenType.ListContinuation, TokenType.List),
        (TokenType.Newline, TokenType.Heading1),
        (TokenType.Newline, TokenType.Heading2),
        (TokenType.Newline, TokenType.Heading3),
        (TokenType.Newline, TokenType.Heading4),
        (TokenType.Newline, TokenType.Heading5),
        (TokenType.Newline, TokenType.Heading6),
        (TokenType.Newline, TokenType.Para),
        (TokenType.Newline, TokenType.ListItem),
    };
    
    /// <summary>
    /// 
    /// </summary>
    /// <param name="this_">the main anlyser doing all this</param>
    /// <param name="tokenType">an end state for which the matching start data will be searched</param>
    /// <param name="stackData">the data about which the question is being asked:
    /// Is this token data and can the token that it would be used to build be a child
    /// of the current token or does it constitute start data corresponding to the end state?</param>
    /// <returns>
    /// "matching" may not be the best term.  the tokenType (of the completed token or current state)
    /// is said to match if any data found could constitute a child token.
    /// </returns>
    private static bool AreMatchingTokenTypes(Analyser this_, TokenType tokenType, StackData stackData)
    {
        if (stackData.DataType == StackDataType.Token)
        {
            return false;
        }

        return matchingTokens.Contains((tokenType, stackData.TokenData.TokenType));
    }

    // see EnhancedText tests for the rules here
    private static void PushTextToken(Analyser this_, TokenType type)
    {
        string text = new string(this_.chars, this_.fragmentStart, this_.idx - this_.fragmentStart);

        // I'm not sure why Newline is a special case but it is insistent.
        text = text.Trim('\n');
        TokenType parentType = TokenType.Invalid;
        if (text.StartsWith(UNDERSCORE_CHAR) && text.EndsWith(UNDERSCORE_CHAR))
        {
            parentType = TokenType.Italic;
            text = text.Substring(1, text.Length - 2);
            if (text.StartsWith(UNDERSCORE_CHAR) && text.EndsWith(UNDERSCORE_CHAR))
            {
                text = text.Substring(1, text.Length - 2);
                parentType = TokenType.Strong;
            }
        }

        Token token = new Token(text);
        if (parentType != TokenType.Invalid)
        {
            token = new Token(parentType, new List<Token>{token});
        }
        this_.tokenStack.Push(new StackData(token));
        Bookmark(this_, type);
    }

    private static void AddExtraListTransition(Analyser this_)
    {
        MyDebug.Assert(this_.transitionStack.Count == 1);
        this_.transitionStack.Push(extraListTransiations);
    }

    private static void PopTransitions(Analyser this_)
    {
        MyDebug.Assert(this_.transitionStack.Count > 1);
        this_.transitionStack.Pop();
    }

    private static void Bookmark(Analyser this_, TokenType tt)
    {
        this_.fragmentStart = this_.GetBookmarkIdx();
    }
    private static void BookmarkAndAddEnhancedTextTransitions(Analyser this_, TokenType tt)
    {
        Bookmark(this_, tt);
        AddEnhancedTextTransitions(this_, tt);
    }

    private static void AddEnhancedTextTransitions(Analyser this_, TokenType tt)
    {
        MyDebug.Assert(this_.transitionStack.Count < 3);
        this_.transitionStack.Push(enhancedTextTransitions);
    }



    
    /// <summary>
    /// This dictionary represents a state machine.
    /// The key comprises the current state and a transictioning "event" in the form
    /// of the next character read from the input.
    /// The looked up value comprises a new state and an action to be taken when exiting the current state.
    /// 
    /// pushing to tokenStack:  the token data is pushed to tokenStack on the transistion
    /// into the state for heading, list and list item (where the token type is entailed by the last character of the token
    /// out of the state for Para, Text, strong and italic where the token is not confirmed until the next char is seen
    /// </summary>
    private static IReadOnlyDictionary<(TokenType, char), (TokenType, Action<Analyser, TokenType>, Action<Analyser, TokenType>)> transitions
        = new Dictionary<(TokenType, char), (TokenType, Action<Analyser, TokenType>, Action<Analyser, TokenType>)>
        {
            {(TokenType.Root, HEADING_CHAR), (TokenType.Heading1Start, Bookmark, Noop)},
            {(TokenType.Root, LIST_CHAR), (TokenType.List, Bookmark, Noop)},
            {(TokenType.Root, TEXT_CHAR), (TokenType.Para, DecrementIdx, Bookmark)},
            {(TokenType.Root, NEWLINE_CHAR), (TokenType.Para, DecrementIdx, Bookmark)},
            {(TokenType.Root, SPACE_OR_TAB_CHAR), (TokenType.Para, DecrementIdx, Bookmark)},
            {(TokenType.Root, UNDERSCORE_CHAR), (TokenType.Para, DecrementIdx, Bookmark)},
            {(TokenType.Root, END_CHAR), (TokenType.End, Noop, Noop)},     // the end is the end there is nothing else!
            {(TokenType.Root, OTHER_CHAR), (TokenType.End, Error,Noop)},    // there are no other chars!
            
            {(TokenType.List, SPACE_OR_TAB_CHAR), (TokenType.ListItemStart, (this_, tt) =>
            {
                PushTokenDataPlus(this_, tt);
                this_.Unread(2);
                AddExtraListTransition(this_);
            },Noop)},
            {(TokenType.List, END_CHAR), (TokenType.Para, (this_,tt) => this_.Unread(2), Noop)},
            {(TokenType.List, OTHER_CHAR), (TokenType.Para, (this_,tt) => this_.Unread(2), Noop)},
            
            {(TokenType.Heading1Start, HEADING_CHAR), (TokenType.Heading2Start, Noop, Noop)},
            {(TokenType.Heading1Start, SPACE_OR_TAB_CHAR), (TokenType.Heading1, Noop, PushTokenDataPlus)},
            {(TokenType.Heading1Start, OTHER_CHAR), (TokenType.Para, (this_, tt) => this_.Unread(2), Noop)},
            {(TokenType.Heading2Start, HEADING_CHAR), (TokenType.Heading3Start, Noop, Noop)},
            {(TokenType.Heading2Start, SPACE_OR_TAB_CHAR), (TokenType.Heading2, Noop, PushTokenDataPlus)},
            {(TokenType.Heading2Start, OTHER_CHAR), (TokenType.Para, (this_, tt) => this_.Unread(3), Noop)},
            {(TokenType.Heading3Start, HEADING_CHAR), (TokenType.Heading4Start, Noop, Noop)},
            {(TokenType.Heading3Start, SPACE_OR_TAB_CHAR), (TokenType.Heading3, Noop, PushTokenDataPlus)},
            {(TokenType.Heading3Start, OTHER_CHAR), (TokenType.Para, (this_, tt) => this_.Unread(4),Noop)},
            {(TokenType.Heading4Start, HEADING_CHAR), (TokenType.Heading5Start, Noop, Noop)},
            {(TokenType.Heading4Start, SPACE_OR_TAB_CHAR), (TokenType.Heading4, Noop, PushTokenDataPlus)},
            {(TokenType.Heading4Start, OTHER_CHAR), (TokenType.Para, (this_, tt) => this_.Unread(5), Noop)},
            {(TokenType.Heading5Start, HEADING_CHAR), (TokenType.Heading6Start, Noop, Noop)},
            {(TokenType.Heading5Start, SPACE_OR_TAB_CHAR), (TokenType.Heading5, Noop, PushTokenDataPlus)},
            {(TokenType.Heading5Start, OTHER_CHAR), (TokenType.Para, (this_, tt) => this_.Unread(6),Noop)},
            {(TokenType.Heading6Start, SPACE_OR_TAB_CHAR), (TokenType.Heading6, Noop, PushTokenDataPlus)},
            {(TokenType.Heading6Start, OTHER_CHAR), (TokenType.Para, (this_, tt) => this_.Unread(7),Noop)},

            {(TokenType.Para, END_CHAR), (TokenType.End, Error, Noop)},    // should have been cut short at Root
            {(TokenType.Para, NEWLINE_CHAR), (TokenType.Newline, PushTokenDataPlus, Noop)},
            {(TokenType.Para, UNDERSCORE_CHAR), (TokenType.EnhancedTextStarting, (this_, tt) =>
            {
                PushTokenData(this_, tt);
                BookmarkAndAddEnhancedTextTransitions(this_, tt);
            }, Noop)},
            {(TokenType.Para, SPACE_OR_TAB_CHAR), (TokenType.SpaceOrTab, (this_, tt) =>
            {
                PushTokenDataPlus(this_,tt);
                this_.fragmentStart = this_.GetBookmarkIdx();
                DecrementIdx(this_, tt);
            },Noop)},
            {(TokenType.Para, OTHER_CHAR), (TokenType.Text, (this_, tt) =>
            {
                PushTokenDataPlus(this_,tt);
                this_.fragmentStart = this_.GetBookmarkIdx();
                DecrementIdx(this_, tt);
            },Noop)},
            {(TokenType.Heading1, END_CHAR), (TokenType.End, Noop, Noop)},
            {(TokenType.Heading1, NEWLINE_CHAR), (TokenType.Newline, Bookmark, Noop)},
            {(TokenType.Heading1, UNDERSCORE_CHAR), (TokenType.EnhancedTextStarting, BookmarkAndAddEnhancedTextTransitions, Noop)},
            {(TokenType.Heading1, SPACE_OR_TAB_CHAR), (TokenType.SpaceOrTab, Bookmark, Noop)},
            {(TokenType.Heading1, OTHER_CHAR), (TokenType.Text, Bookmark, Noop)},
            
            {(TokenType.Heading2, END_CHAR), (TokenType.End, Noop, Noop)},
            {(TokenType.Heading2, NEWLINE_CHAR), (TokenType.Newline, Bookmark, Noop)},
            {(TokenType.Heading2, UNDERSCORE_CHAR), (TokenType.EnhancedTextStarting, BookmarkAndAddEnhancedTextTransitions, Noop)},
            {(TokenType.Heading2, SPACE_OR_TAB_CHAR), (TokenType.SpaceOrTab, Bookmark, Noop)},
            {(TokenType.Heading2, OTHER_CHAR), (TokenType.Text, Bookmark, Noop)},
            
            {(TokenType.Heading3, END_CHAR), (TokenType.End, Noop, Noop)},
            {(TokenType.Heading3, NEWLINE_CHAR), (TokenType.Newline, Bookmark, Noop)},
            {(TokenType.Heading3, UNDERSCORE_CHAR), (TokenType.EnhancedTextStarting, BookmarkAndAddEnhancedTextTransitions, Noop)},
            {(TokenType.Heading3, SPACE_OR_TAB_CHAR), (TokenType.SpaceOrTab, Bookmark, Noop)},
            {(TokenType.Heading3, OTHER_CHAR), (TokenType.Text, Bookmark, Noop)},
            
            {(TokenType.Heading4, END_CHAR), (TokenType.End, Noop, Noop)},
            {(TokenType.Heading4, NEWLINE_CHAR), (TokenType.Newline, Bookmark, Noop)},
            {(TokenType.Heading4, UNDERSCORE_CHAR), (TokenType.EnhancedTextStarting, BookmarkAndAddEnhancedTextTransitions, Noop)},
            {(TokenType.Heading4, SPACE_OR_TAB_CHAR), (TokenType.SpaceOrTab, Bookmark, Noop)},
            {(TokenType.Heading4, OTHER_CHAR), (TokenType.Text, Bookmark, Noop)},
            
            {(TokenType.Heading5, END_CHAR), (TokenType.End, Noop, Noop)},
            {(TokenType.Heading5, NEWLINE_CHAR), (TokenType.Newline, Bookmark, Noop)},
            {(TokenType.Heading5, UNDERSCORE_CHAR), (TokenType.EnhancedTextStarting, BookmarkAndAddEnhancedTextTransitions, Noop)},
            {(TokenType.Heading5, SPACE_OR_TAB_CHAR), (TokenType.SpaceOrTab, Bookmark, Noop)},
            {(TokenType.Heading5, OTHER_CHAR), (TokenType.Text, Bookmark, Noop)},
            
            {(TokenType.Heading6, END_CHAR), (TokenType.End, Noop, Noop)},
            {(TokenType.Heading6, NEWLINE_CHAR), (TokenType.Newline, Bookmark, Noop)},
            {(TokenType.Heading6, UNDERSCORE_CHAR), (TokenType.EnhancedTextStarting, BookmarkAndAddEnhancedTextTransitions, Noop)},
            {(TokenType.Heading6, SPACE_OR_TAB_CHAR), (TokenType.SpaceOrTab, Bookmark, Noop)},
            {(TokenType.Heading6, OTHER_CHAR), (TokenType.Text, Bookmark, Noop)},

            {(TokenType.ListItemStart, END_CHAR), (TokenType.End, Error, Error)},    // should have been handled by List or ListContinuation
            {(TokenType.ListItemStart, LIST_CHAR), (TokenType.ListItem, Bookmark ,Noop)},
            {(TokenType.ListItemStart, OTHER_CHAR), (TokenType.Root, (this_,tt) => this_.Unread(2), Noop)},
            
            {(TokenType.ListItem, SPACE_OR_TAB_CHAR), (TokenType.SpaceOrTab, PushTokenData, Bookmark)},
            {(TokenType.ListItem, OTHER_CHAR), (TokenType.Root, (this_, tt) => this_.Unread(2), Noop)},

            {(TokenType.Text, SPACE_OR_TAB_CHAR), (TokenType.SpaceOrTab, Noop, Noop)},
            {(TokenType.Text, NEWLINE_CHAR), (TokenType.Newline, PushTextToken, Noop)},
            {(TokenType.Text, END_CHAR), (TokenType.End, PushTextToken, Noop)},
            {(TokenType.Text, OTHER_CHAR), (TokenType.Text, Noop, Noop)},
            
            {(TokenType.SpaceOrTab, SPACE_OR_TAB_CHAR), (TokenType.SpaceOrTab, Noop, Noop)},
            {(TokenType.SpaceOrTab, UNDERSCORE_CHAR), (TokenType.EnhancedTextStarting, (this_, tt) =>
            {
                PushTextToken(this_, tt);
                Bookmark(this_, tt);
                AddEnhancedTextTransitions(this_, tt);
            }, Noop)},
            {(TokenType.SpaceOrTab, NEWLINE_CHAR), (TokenType.Newline, PushTextToken, Noop)},
            {(TokenType.SpaceOrTab, END_CHAR), (TokenType.End, PushTextToken, Noop)},
            {(TokenType.SpaceOrTab, OTHER_CHAR), (TokenType.Text, Noop, Noop)},
                        
            {(TokenType.Newline, END_CHAR), (TokenType.End, Noop, Noop)},
            {(TokenType.Newline, OTHER_CHAR), (TokenType.Root, PushToken, (this_, tt)  => this_.Unread(1))},
            
            {(TokenType.End, END_CHAR), (TokenType.End, PushToken, Error)},
            
        };

    /// <summary>
    /// This is required to handle the case of the list token which is multi-line
    /// ******** IMPORTANT *************
    /// If there is no OTHER_CHAR entry in the main transition map for a state
    /// then this map must contain all possible transitions for that state
    /// </summary>
    private static IReadOnlyDictionary<(TokenType, char), (TokenType, Action<Analyser, TokenType>, Action<Analyser, TokenType>)> extraListTransiations
        = new Dictionary<(TokenType, char), (TokenType, Action<Analyser, TokenType>, Action<Analyser, TokenType>)>
        {
            {(TokenType.Newline, LIST_CHAR), (TokenType.ListContinuation, PushToken, Noop)},
            {(TokenType.Newline, OTHER_CHAR), (TokenType.Root, PushToken, Noop)},
            {(TokenType.ListContinuation, SPACE_OR_TAB_CHAR), (TokenType.ListItemStart, (this_, tt) => this_.Unread(2), Noop)},
            {(TokenType.ListContinuation, NEWLINE_CHAR), (TokenType.Para, (this_, tt) =>
            {
                this_.Unread(2);
                PushToken(this_, tt);
                PopTransitions(this_);
            }, Noop)},
            {(TokenType.ListContinuation, END_CHAR), (TokenType.Para, (this_, tt) =>
            {
                this_.Unread(2);
                PushToken(this_, tt);
                PopTransitions(this_);
            }, Noop)},
            {(TokenType.ListContinuation, OTHER_CHAR), (TokenType.Para, (this_, tt) =>
            {
                this_.Unread(2);
                PushToken(this_, tt);        // the list
                PopTransitions(this_);
            }, Noop)},
        };

    /// <summary>
    /// This is required to handle the case of the list token which is multi-line
    /// ******** IMPORTANT *************
    /// If there is no OTHER_CHAR entry in the main transition map for a state
    /// then this map must contain all possible transitions for that state
    /// </summary>
    private static IReadOnlyDictionary<(TokenType, char), (TokenType, Action<Analyser, TokenType>, Action<Analyser, TokenType>)>
        enhancedTextTransitions
        = new Dictionary<(TokenType, char), (TokenType, Action<Analyser, TokenType>, Action<Analyser, TokenType>)>
        {
            {(TokenType.EnhancedTextStarting, UNDERSCORE_CHAR), (TokenType.Text, Noop, Noop)},
            {(TokenType.EnhancedTextStarting, NEWLINE_CHAR), (TokenType.Text, (this_, tt) =>
            {
                PushTextToken(this_, tt);
                Bookmark(this_, tt);
                PopTransitions(this_);
            }, Noop)},
            {(TokenType.EnhancedTextStarting, END_CHAR), (TokenType.Text, PushTextToken, Noop)},
            {(TokenType.EnhancedTextStarting, TEXT_CHAR), (TokenType.Text, Noop, Noop)},
            {(TokenType.EnhancedTextStarting, SPACE_OR_TAB_CHAR), (TokenType.Text, Noop, Noop)},
            {(TokenType.EnhancedTextStarting, OTHER_CHAR), (TokenType.Text, Noop, Noop)},
            {(TokenType.Text, UNDERSCORE_CHAR), (TokenType.EnhancedTextEnding, Noop, Noop)},
            {(TokenType.Text, NEWLINE_CHAR), (TokenType.Newline, (this_, tt) =>
            {
                PushTextToken(this_, tt);
                Bookmark(this_, tt);
                PopTransitions(this_);
            }, Noop)},
            {(TokenType.Text, END_CHAR), (TokenType.End, PushTextToken, Noop)},
            {(TokenType.Text, SPACE_OR_TAB_CHAR), (TokenType.Text, Noop, Noop)},
            {(TokenType.Text, OTHER_CHAR), (TokenType.Text, Noop, Noop)},
            {(TokenType.EnhancedTextEnding, OTHER_CHAR), (TokenType.EnhancedTextEnding, Noop, Noop)},
            {(TokenType.EnhancedTextEnding, UNDERSCORE_CHAR), (TokenType.EnhancedTextEnding, Noop, Noop)},
            {(TokenType.EnhancedTextEnding, TEXT_CHAR), (TokenType.Text, Noop, Noop)},
            {(TokenType.EnhancedTextEnding, END_CHAR), (TokenType.End, PushTextToken, Noop)},
            {(TokenType.EnhancedTextEnding, NEWLINE_CHAR), (TokenType.Newline, (this_, tt) =>
            {
                PushTextToken(this_, tt);
                Bookmark(this_, tt);
                PopTransitions(this_);
            }, Noop)},
            {(TokenType.EnhancedTextEnding, SPACE_OR_TAB_CHAR), (TokenType.SpaceOrTab, (this_, tt) =>
            {
                PushTextToken(this_, tt);
                Bookmark(this_, tt);
                PopTransitions(this_);
            }, Noop)},
        };    
    
 
    private int GetBookmarkIdx()
    {
        return idx;
    }
}

internal class Token
{
    public TokenType Type { get; }
    public string Text { get; }
    public IList<Token> Children { get; }

    public Token(string text)
    {
        this.Text = text;
        this.Type = TokenType.Text;
    }

    public Token(TokenType tokenType)
    {
        MyDebug.Assert(tokenType == TokenType.Newline);
        this.Type = tokenType;
    }

    public Token(TokenType tokenType, IList<Token> children)
    {
        MyDebug.Assert(tokenType != TokenType.Newline && tokenType != TokenType.Text);
        this.Type = tokenType;
        this.Children = children;
    }

    public string BeforeTag => Type == TokenType.Text ? Text : $"<{GetTag()}>";
    public string AfterTag => Type == TokenType.Text ? string.Empty : $"</{GetTag()}>";

    private Dictionary<TokenType, string> tags = new Dictionary<TokenType, string>
    {
        {TokenType.Heading1, "h1"},
        {TokenType.Heading2, "h2"},
        {TokenType.Heading3, "h3"},
        {TokenType.Heading4, "h4"},
        {TokenType.Heading5, "h5"},
        {TokenType.Heading6, "h6"},
        {TokenType.Italic, "em"},
        {TokenType.Strong, "strong"},
        {TokenType.List, "ul"},
        {TokenType.ListItem, "li"},
        {TokenType.Para, "p"},
    };
    private string GetTag()
    {
        if (tags.ContainsKey(this.Type))
        {
            return tags[this.Type];
        }
        return string.Empty;
    }
}

/// <summary>
/// System.Diagnostics.Debug.Assert really upsets xunit or Microsoft.NET.Test.Sdk
/// to the extent where the test run hangs.  So here is my own sad substitute.
/// Hail, Bertrand Meyer
/// </summary>
internal static class MyDebug
{
    public static void Assert(bool condition, string message = null)
    {
        if (!condition)
            throw new InvalidOperationException(message);
    }
}
