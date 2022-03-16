Unicode code point strings
==========================

C++ lacks decent support for Unicode strings which can be accessed as a sequence of code points. The u8string, u16string
instantiations introduced in C++17 implement strings with known utf8 and utf16 encodings respectively, but still has no means
to access or advance in units of code points. Furthermore, as these strings are mutable on the character level it is easy to create
an invalid string by poking around. The only way to work with unicode code point strings in C++20 is to use u32string but this is
very wasteful of storage. There is also no standardized way to convert u16strings to u32strings (although there is one for
u8strings).

If strings were immutable it would be easier to work in code points as there is no need to replace a character with another which
requires a different number of bytes. Random accessing the code points is also problematic. If these two features are removed a new
ustring class which can store code point sequences in different encodings would be possible. The ustring described below is such a
class.

Is mutablility and random access important?
-------------------------------------------

In my experience string mutation is seldom needed and random access is also seldom used. Instead most string manipulation uses the
operations find, substr and operator+, that is, finding substrings, extracting substrings and concatenating strings. With a copyable
iterator finding is easy to implement without random access. rfind can be implemented the same way given that the end iterator can
be decremented to point at the last character of each string. With reversible encodings this is entirely possible. Substring
creation is easily achievable if specified by iterators, must as when creating a span. A simple implementation could store two
iterators along with a pointer to an implementation object. However, as the iterator class itself needs to know the encoding as well
as the byte position there are more optimal ways to store iterator positions in ustrings than to actually store iterators.

Short strings can be optimized into the ustring representation and by smart coding the begin and end pointers wrt to virtual
addresses not really using all 64 bits (for the forseeable future) the 24 byte representation of the long string format consisting
of start, end and implementation pointers can be overlaid with a 23 byte string storage area and one mode byte. By letting 00 and FF
mode bytes both mean long string with impl pointer no bit manipulation is required to extract the last pointer (or first for big
endian architectures, if such still exist).


Avoiding copies of literals
---------------------------

In addition to short strings with 23 bytes or less of data there is a large number of cases where we don't really need to copy the
string contents at all, namely string literals. String literals are very prevalent a large percentage of the std::strings created in
a typical C++ program actually hold copies of string literals. string_view helps somewhat but this requires the code using the
string to use string_view. This proposal contains user defined literals with the u suffix which return ustrings directly refering to
the literal characters (u8, u16, u32 encodings and probably a local code page on windows for the regular literal (execution
encoding)).


Reading external data of any encoding
-------------------------------------

Reading text from files and network connections can be troublesome to get efficient, especially when variable width encodings are
used. For network transfers (and some file formats) it is impossible to know even the byte length of the encoded characters when
reading starts. The ustring class has a helper class ustring_builder which can be used in this type of case. The ustring_builder
class is constructed with an encoding and an initial size. The encoding can include codecvt based encodings. After filling the
initial buffer fully or partially with encoded bytes the program can call for a new buffer to fill. The ustring_builder can opt to
extend the buffer, to convert what it has into some internal format or add another buffer for the program to fill. When all of the
data has been added to the ustring_builder it can be used to construct a ustring. After this the ustring_builder can not be used
anymore. If the incoming data is of a format suitable for ustring's direct use ustring_builder will probably create a ustring::impl
object directly but it might also opt to allocate an array of buffers so that the number of bytes required for the full
ustring::impl can be exactly known when is to be allocatd. The problem with codecvt is that even with this done it is impossible to
know how long buffer the actual ustring will need without inspecting the actual bytes. In this case it may be better to convert the
buffer the program wrote to to an internal format buffer by buffer so that the single long buffer used by the actual ustring can be
allocated to size.

As ustrings are immutable and not random accessible it would also be possible to allow fragmented storage of the ustring itself,
i.e. that its implementation object could have a mode where it refers to several subordinate blocks. For the purpose of reading an
unbounded amount of bytes from e.g. a network connection all of the blocks would have the same encoding, but this system could also
be useful for string concatenation where each part could possibly have different encoding.

With concatenation also comes the possibility of nested sub-blocks, i.e. when the result of concatenating several strings is itself
used as a argument in another concatenation. This causes a problem for iterators, as these must be able to point to the encoded code
point at any level and figure out how to advance over part borders in any number of levels. The key here may be to make sure that
"any" is not really infinite but bounded so that any length strings can be handled. One observation is that a string is at least one
character and consumes one byte. This means that the maximumm total string length is 2^64. Furthermore, any part going into the
concatenation and having a representation less than 22 bytes in length will be stored in line in the substring and thus not cause
another level, so we can count on all blocks on the leaf level to contain at least 23 bytes of valid character data. Thus you may
think that given a minimum fan out of, say, 16 strings per level, akin to a B-Tree you would get a maximum number of levels to keep
track of in the iterator. Indeed, four bits per level would be enough. If 64 bits are reserved this gives us 16 levels. As each
level refers to a block of 16*24 bytes we quickly consume all addressable memory even if concatenation starts out with only 23 byte
strings.

The worst case scenario the first 4 bits address 23*16 + 24*16 = 752 bytes, and each nibble addresses 16 times more. About 10 levels
fills the 56 bits of virtual adddresses availabel in any processor.

While this is easily achieved for the file/network loading case the programmatic concatenation case is much more problematic. For
instance two strings of 23 bytes can be concatenated and stored. Then another 23 byte string is added while an external refcount of
the first combination is kept. This creates a new level pointing to the first pair and the single second string. Repeat and a very
unfavorable structure is created. As long as no external refcounts are retained this could be handled by rearranging behind the
scenes but in case of external references a copy on write would be required to keep the tree balanced. This, on the other hand, has
other advantages.

Regular string concatenation most often has <22 bytes per part mixed with possibly longer literals which means that flattening the
result into one buffer is reasonable anyway. With a fan out of 16 we have 24*16 bytes in the block and most often the sum of the
concatenated string would be less than that. There are many options. 


Concatenation using expression templates
----------------------------------------

It is very tempting to let operator+ return a class encoding the data type of each part, and then have a constructor for ustring
which takes any of these classes and inspects them to figure out the total length and a suitable encoding. The problem with this is
that if you use auto str = a + "Hej"u; str has some weird expression template type rather than ustring. If you use this as the
parameter to some template function the entire template will be passed, possibly causing strange errors in later instantiation
steps. One way to make it almost impossible to use auto in this way (std::ustring isn't that hard to type anyway!) is to not let
there be any useful methods on the expression template classes themselves, and making them totally uncopiable/unmovable. The only
remaining issue would be that there must of course still be an operator+ to append a further string part. This may be a nice idea
though.

One especially problematic error mode would be to have a function or lambda return auto and then return a concatenation nominally
yielding a ustring. Then the function actually returns some expression template type. If there are multiple returns the return type
would most likely be different (depending on how much info is encoded in the template instantiations).

The simplest possible setup would be to just have two operator+ and one template class:


    template<size_t N> class uconcatenated {
    public:
        uconcatenated<N - 1> prefix;
        ustring part;
    };

    template<> class uconcatenated<2> {
    public:
        ustring part1;
        ustring part2;
    };

    uconcatenated<2> operator+(ustring a, ustring b ) {
        return { a, b};
    }

    template<size_t N> uconcatenated<N + 1> (uconcatenated<N> a, ustring b) {
        return { a, b };
    }

By not encoding anything else than the count in the template type the number of template instantiations is minimized. A template
function which can flatten the structure into one ustring should be doable.

If ustrings can support a split implementation which is interesting for long literals mostly this function will have to make some
though choices. Those choices could be moved at least partly to compile time if the uconcatenated (and embedded ustrings) were more
elaborate with for instance overloads for literal + ustring and vice versa (without using u-literals). To ensure that the ltieral is
actually a literal some other custome literal letter would have to be used. This is not reasonable, so instead a u suffix may not
actually return a ustring but some object that an ustring can be constructed from, basically a literal_span, or why not a
string_view (reusing the sv literals). That is, a string_view's characters may be referred to from an ustring, even after
concatenation with something else! No, better with a literal_span which is never generated from anything else than literals.

The auto variable problem is retained here, unless the proposal for operator auto would proceed finally, which seems unlikely.

If the literal's length is encoded in the type, which is possible in C++20, the compile time part of the decision making machinery
would have more information at hand. But as soon as any part of the concatenation is runtime variable (and why else would you
concatenate at all?) those parts would have to be assumed unless each to_string returned a type indicating the typical string length
based on the data size, or at least one that indicates "probably less than 23 bytes". Concatenating more than two of those still
poses a problem. Mainly, however, the issue would be if there are long literals we don't want to copy or if there are parts of
different encodings, for instance inserting regular to_string returns into a wide literal of some kind.

The main issue to solve is the classic immutable + concatenation question, how to avoid massive numbers of copies. Literals and soo
solves most of that even if no multi-part storage mode is implemented.

Another possibility would be to use a type-erased form of uconcatenated. This only works for the N * ustring case but basically
means to just build a multi-representation. In the basic form each concatenation must allocate a new impl object with room for one
more entry but by utilizing rvalue semantics and checking refcount == 1 most of these can be avoided. There will be one allocation
though, even if the total length turns out to be less than 23 bytes. This can be avoided if operator+ tries to reuse any rvalue
ustring in soo mode by packing the other ustring contents into it. If this is not possible a choice must be made whether to create a
shared representation and copy both operands into it or whether to create a multi-representation. A shared representation could also
have spare room for expansion although reserving any substantial amount would be wasteful for all the cases when no concatenation
occurs while unshared. Another possibility could be to reserve a fixed number of bytes for the shared reprensentation. Then a
regular shared implementation could start from the front while a multi-representation could be built from the back end. Such blocks
could be allocated from a thread specific array, i.e. fairly quickly. If they are always only used temporarily they could even be
allocated from a side stack. I don't know where this is going... but the main issue seems to be that we don't know how many
substrings are to be concatenated before the complete string is starting to be used. There are many variables that affect which
strategy is the best, including:

- The number of concatenations.

- The lengths of the different parts.

- Which parts are literals.

- Which parts are variable lengths.

- What encodings are used by the different parts.

- How much is the string used before it is discarded.

Of these the number of concatenations, which substrings are literals and which encodings are used are knowable at compile time.
However, it does not seem possible to exploit this knowledge and still end up with ustring as the actual type of the concatenated string.

A sraight-forward implementation would be to let operator+ always concatenate using a multi-representation, maybe using a side
stack. If we can guarantee that the side stack is reclaimable whenever the string is used this is ok. The mandatory and allowed copy
elisons in later C++ standards may hinder this, but I'm not sure. There are two basic problems: call and return.

In the return case a function returns a ustring by value. The return statement expression uses concatenation. Then the copy/move
construction must be elided so the caller will end up with having the return value of operator+ as this is contructed in place. Does
this fuck up the side stack? I don't think so. There are two principal cases:

    ustring f() { return "a" + "b"; }

    // Direct capture
    ustring x = f();        // x contains the concatenated representation which is still on the side stack.

    // Forwarded return:
    ustring g() { return f(); } // The actual implementation will send a pointer to the receiving ustring object in through the call chain so this is the same.

What we want to guarantee is that we can't start using the side stack again for another concatenation before the last ustring is
removed from it. This is clearly not fulfilled for this function:

    ustring h() {
        ustring ret = f();
        ustring q = "1" + "2";
        return ret;
    }

The question here is whether the move construction in the return statement would actually be elided for a non-trivial move
constructor like this. OTOH, due to the detruction order q would be destructed first, popping its parts from the side stack.
This function is worse:

    ustring i() {
        ustring ret = f();
        ustring q = "1" + "2";
        ret += "c";
        return ret;
    }

Now the stack entries are intertwined and when q is deallocated it can't pop the stack beyond the 'c' part. Also, the implementation
may require the parts of ret to occupy adjacent cells in the side stack. A way out could be the observation that operator+= would
actuall "use" the lhs and thus merge it to a "real" implemenation. The problem is that q is above it on the stack at this point, so
it can't realistically be a pure side stack. All its entries would however eventually be freed so a simple allocator would do. When
operator+ sees that its lhs is on the stack it can check if it is on the top and if not and full it could realize it and then start
a new slot (or reuse the old one. If each slot has room for say 8 parts this would very rarely happen anyway. As it is a stack like,
cheap allocation time structure it does not waste much to allocate 8 parts even though you only saw two.

So, is there a risk that a ustring whose representation is still on the side stack could be operated on by another thread.
Unfortunately this does not seem to be possible to exclude. Even if the copy constructor/operator would always realize the complete
ustring and set both the original and the copy to a fully realized ustring there is still the risk that another thread is the first
to access the ustring in a way that must realize it, thus defeating the hope that the allocation could be thread-local.

### A simple concatenation strategy

To make this implementable I'm going to start with a fairly simple strategy. Apart from the rules above we also have to consider the
rvalueness of the lhs and rhs. The regular concatentation would have a rvalue on both sides but as soon as one side is a ustring
variable it is a lvalue (obvious observation). This calls for four overloads. Strategywise there are three cases, zero, one or two
lvalues.

In all these cases we first check if both strings have the same encoding and the concatenated length (in bytes) fits in the soo
mode. If so go for soo and return by value the concatenated string. We use the same tactic if one string is utf8 and the other
ascii, as ascii is a subset of utf8. There are some other combinations too in these cases we can't just sum up the byte counts to
see if the concatenation fits in the soo block so lets skip them for now. Actually one table8 and one ascii may also work, given
that the table starts with ascii, which should be a readable attribute.

Short literals that fit in soo should probably be treated as soo, the copying work is the same, but the accessing may be a bit
faster as there is one less indirection level to traverse in the soo mode.

As ustrings are immutable we never allocate spare room for more data. This prevents concatenating by using any remaining trailing
bytes. Adding extra bytes just in case we have a concatenation seems suboptimal, but with metaprogramming we may be able to gain
knowledge and allocate an approximate or maximum size. Such meta code might even see that a to_string or similar function with a
maximum resulting string length is present and adjust allocation for that.

Without metaprogramming and without using expression templates (to avoid the bloat and the risk of auto variables not being ustrings
when they should) the only remaining possible strategies is to either do the concatenation on the spot, into a new shared
representation or to keep a multi-string representation. The selected strategy always creates a multi-string in this case. This
avoids multiple increasing length allocation when multiple parts are concatenated.

Whenever a multi-part is created it could get a fixed number of parts, probably 4 or 8 would be a nice compromise. Could also be 3
or 7 or something like that to bring the total byte count to for instance 128. As each ustring has its own start and end pointers we
could continue concatenating even if the refcount > 1 as the old references will never know that the newly added parts exist.

If such a mutli-part overflows another group of parts can be added, pointed to by the last entry. Or maybe this is an extra pointer
to the next block. This is probably better, and the next block can be larger. Alternatively a larger block is allocated and the new
ustring representing the longer concatenation points at this one. Blocks can double each time to reduce the number of reallocations
as usual.

If a concatenation has a short string on the rhs and the lhs is already a multi-string we can still do the concatenation into the
last part if it has soo mode and room enough. Any pre-exsisting ustrings or iterators would not care, we're just adding bytes beyond
where they would ever look.

Well, this may not be true after all: If two ustrings point to the same implementation then both can do independent concatenations
so we can't allow this to happen unless the refcount is 1.

operator= for ustring, when the rhs has refcount == 1, could coerce the sum of the concatenation into a single string with one
encoding to save time later when accessing. In reality it is not knowable whether this is a good tactic in each particular case, but
some heuristic can probably be worked out. One idea would be to check if the flattening can be done in situ, saving an allocation.
However, if we don't want to move the multi-string parts before doing so it gets tricky. The only viable case to calculate is when
all parts have the same encoding (with some additions as above) and the sum of the byte counts up to part n is never large enough to
destroy the part itself. If each ustring is 24 bytes this means that the first part can't consume more than 24 bytes, the sum of
part 1 and 2 not more than 48 and so on. In this case we can do the flattening by taking the members of each part into local
variables before copying the characters.

If this size criterion is not met we have fairly long strings and thus it may be wiser to leave them shared.

operator+= would operate differently both depending on if the lhs is already a multi-part and whether it has a refcount > 1. Well,
it basically operates by first doing operator+ and then assigning this to the lhs ustring. 

Note that _each_ part in a multi-part ustring-impl is a complete ustring so it can refer to a part of the underlying ustring. This
never has any implication on the possibilities for flattening as flattening is just to take the active part and storing it somewhere
else. %%What if the part being flattened is a part of an old multi-part, with its start and end pointers pointing in different old
parts. When flattinging, again, this doesn't matter. But what might be complicated is to calculate the byte size in this case.
Should not be really hard though.



The ustring solution
--------------------
The std::ustring class is a immutable string dealing directly in unicode code points. It can be constructed from many types of
strings and may be implemented to convert incoming data on construction or on use. The API is designed to allow even non-reversible
conversion to happen on the fly.

There is no random accessing as this would prevent many strategies for conversion, also there is seldom a need to random access
strings, either you progress forward or use a predefind find function. This would mean that the only way to get an iterator would be
to call begin() and then call advance with a O(N) performance guarantee. At the same time, to be able to construct an ustring from a
pair of iterators, while still preserving the possibility to use reference counting offered by the immutability the iterator must
also hold a reference to the implementation. This has lead to the idea that there are no iterators, instead an ustring acts
simultaenously as an iterator. When you advance an ustring it looses the knowledge of the passed characters and can not be backed up.

To offer reversible iteration and random access for the cases when it is needed methods which return an ustring guaranteed to
support these modes of access can be created by method calls. If the original ustring already has the requested ability (for
instance an utf8 implementation is reversible but not random-accessible) these calls just return a copy, while if not it performs
the conversion as needed, which for longer strings involves creating a converted implementation string. It would be an alternative
to return subclasses from such methods, which add functionality, but do we really want to add even more string classes?

An ustring object needs to be at least 24 bytes (for 64 bit builds) which is the same as a std::string in most compilers. In the
case that the incoming data needs a converter (codecvt, icu or iconv based implementations are feasible) an intermediate heap object
containing a buffer for a converted part of the source string is needed.

The basic operation after contsruction is to use find or operator* and operator++ to find a suitable position in a ustring. Then
this can be used to crate substrings before, after, between or outside iterators. As ustrings are their own iterators these APIs are
a bit unfamiliar:

    class ustring {

    ...
    ustring find(ustring pattern);
    ustring front(ustring pos);


As the ustring returned by find represents the rest of the original string just using the ustring returned by find is equivalent to
substr(pos) for a string while:

    ustring str;
    // Left
    ustring first_part = str.find(pattern);
    // Right
    ustring last_part = str.front(str.find(pattern));

    // Mid:
    ustring first_comma = ++str.find(',');
    ustring between_commas = first_comma.front(first_comma.find(','));

    // Erase
    ustring first_comma = str.find(',');
    ustring outside_commas = str.front(first_comma) + (++first_comma).find(',');


A range based for on an ustring uses the begin and end functions, which are overloaded to return a copy of the string itself for
begin and a default constructed ustring for end. As a ustring knows if it is at the end (although it may not know how many code
points remain when it's not) it can always know if it is supposed to compare equal to the sentinel. As a copy is returned advancing
it does not affect the original string so the iteration loops over the string contents without affecting the original's starting
point.

As the ustring is immutable concatenation is a bigger problem than for std::basic_string. However, this is much reduced by a
multipart storage model which can be employed when concatenating multiple ustrings. As concatenated ustrings can have different
encodings actually coercing such strings into one string can be complicated. Also, when concatenating multiple strings which each
fall inside the soo length a check to see if the sum also does is preferred. As all numeric to string conversions fit inside the soo
length (22 chars) conversion to ustring never needs any heap allocation.

Implementations can of course skip any of these possiblities and for instance always store strings as utf8, utf16 or only use a few
different encodings. Especially using codecvt, icu or iconv is problematic as it requires a buffer to store the output, which is of
indeterminate length. If constructors that take locale convert immediately this allows bidirectional access always, it may be a good
idea to enforce this as doing the conversion on the fly when a ustring instance advances requires separate conversion for each
ustring instance (no sharing) and thus the conversion may be done multiple times for the same data. Simple and reversible encodings
such as utf8 and utf16 and table lookup (isolatin etc.) is probably best done on the fly.

This limitation means that only to get random access we need to do a special conversion call. As ustring deals in code points this
must be to utf32 or some internal form such as utf20 or utf24 depending on the speed/size trade off required. As stated before
random access is seldom useful, but on the other hand size is so maybe an idea of a "original index" is good. The original index is
the number of code points from the start of the original string that was converted to ustring. This is knowable by all ustrings
except default constructed ones (which are used as sentinels).

ustring::advance(size_t) naturally can only advance forwards as the ustring does not know where it once started, only its current
position and its end. This seems silly as the shared implementation is still valid, and if the implementation is not shared it is
most likely faster to copy the entire 24 byte block than to try to shuffle bytes around, especially if the encoding is variable
length such as utf8.

The fact that a ustring is an iterator is thus self contradictory in that it can never go backwards, as far as the ustring is
concerned there is nothing there. It seems more fruitful to think of a ustring as a container with a cursor. The cursor position is
the iterator position which can be read and moved in both directions. This extends to the ends of the original string from which it
was created. However, in many cases we really do want to limit the extent that the cursor can traverse which for shared storage
requires four pointers, one to the shared storage (to maintain the reference count and get the encoding etc. as well as three
positions of the allowed subinterval, start, current and end. The current index is then within this substring.

Again it seems that making ustring its own iterator is not a good idea after all. Mainly because it is unorthodox in C++ but also
for practical reasons. If we disallow storing data using a forward only or stateful encoding iterators don't get so darned complex
and there is little double work done when advancing multiple iterators. The problem that is retained is with multi-part ustrings.
This still requires the iterartors to maintain both a pointer to the impl object (which has the array of parts that constitutes the
whole string) and 