/*


What's the universe of values a pure-vanilla program can manipulate?
- rationals
- booleans: true and false
- lists: empty and cons
- strings
- tagged values (including syntax)
- functions

*/



Base = {};


Base.arity = (f) => function() {
    if (arguments.length !== f.length)
        throw Error("Function " + f +
                    " expected " + f.length +
                    " arguments, but got " + arguments.length +
                    " instead; namely: " + arguments);
    return f.apply(this, arguments);
};
Base.error = Base.arity(exn => { throw Error(exn); });

// booleans
Base["true"] = true;
Base["false"] = false;
__ormap = (arr, pred) => arr.every(v => pred(v));


// numbers / rationals
Base.isNumber = Base.arity(v => bigRat().constructor === v.constructor);
Base["+"] = (...args) => {
    if (!__ormap(args, Base.isNumber))
        Base.error("plus non-number");
    return args.reduce((x, y) => x.plus(y), bigRat(0));
};
Base["-"] = (...args) => {
    if (!__ormap(args, Base.isNumber))
        Base.error("minus non-number");
    if (args.length === 1)
        return bigRat(0).minus(args[0]);
    return args.slice(1).reduce((x, y) => x.minus(y), args[0]);
};
Base["<"] = Base.arity((x, y) => {
    if (!__ormap([x, y], Base.isNumber))
        Base.error("less-than non-number");
    return x.lesser(y);
});


// lists
Base.isEmpty = Base.arity(v => v === null);
Base.Pair = function(first, rest) {
    this.first = first;
    this.rest = rest;
};
Base.isPair = Base.arity(v => v.constructor === Base.Pair);
Base.cons = Base.arity((hd, tl) => new Base.Pair(hd, tl));
Base.first = Base.arity(v => {
    if (!Base.isPair(v))
        Base.error("first non-pair")
    return v.first;
});
Base.rest = Base.arity(v => {
    if (!Base.isPair(v))
        Base.error("rest non-pair")
    return v.rest;
});
Base.list = (...args) => {
    return args.reduceRight((lst, elem) => Base.cons(elem, lst), null);
};


// strings
Base.isString = Base.arity(v => typeof v === "string");
Base.split = Base.arity((string, delim) => {
    if (!Base.isString(string) || !Base.isString(delim))
        Base.error("split non-string");
    return string.split(delim);
});
Base.slice = Base.arity((string, start, end) => {
    if (!Base.isString(string))
        Base.error("slice non-string");
    if (!Base.isNumber(start) || !Base.isNumber(end))
        Base.error("slice non-number");
    if (start.denom.neq(1) || end.denom.neq(1))
        Base.error("slice non-integer")
    start = start.num;
    end = end.num;
    if (start.lt(0)) start = bigInt(0);
    if (end.lt(0)) end = bigInt(0);
    if (start.gt(string.length)) start = bigInt(string.length);
    if (end.gt(string.length)) end = bigInt(string.length);
    // now that we know start and end are within [0, length],
    // we also know they are representible by a JS integer!
    if (typeof start.value !== "number" || typeof end.value !== "number")
        throw Error("Base.slice is broken!");
    return string.slice(start.value, end.value);
});
Base.parseInt = Base.arity(v => {
    if (!Base.isString(v))
        Base.error("parseInt non-string")
    return bigRat(bigInt(v));
});
Base.strcat = (...args) => {
    if (!__ormap(args, Base.isString))
        Base.error("strcat non-string");
    return args.join("");
};

Base.isFunc = Base.arity(v => typeof v === "function");


// generic

// TODO: implement observational equality in terms of something else?
// === implements observational equality:
//  - atoms are compared by value
//  - lists are compared recursively by value
//  - functions... are
Base["!=="] = Base.arity((x, y) => !Base["==="](x, y));
Base["==="] = Base.arity((x, y) => {
    if (x === y) return true; // covers true, false, null, string, and some objects
    if (Base.isNumber(x) && Base.isNumber(y)) return x.eq(y);
    if (Base.isPair(x) && Base.isPair(y)) {
        return Base["==="](x.first, y.first)
            && Base["==="](x.rest, y.rest);
    }
    
});
Base["!="] = Base.arity((x, y) => !Base["=="](x, y));
// - TODO friendlyequal has to be different for syntax
Base["=="] = Base.arity((x, y) => Base["==="](x, y));
Base.length = Base.arity(v => {
    if (Base.isString(v))
        return bigRat(v.length);
    if (Base.isPair(v) || Base.isEmpty(v)) {
        var len = 0;
        while (Base.isPair(v)) {
            len++;
            v = v.rest;
        }
        return bigRat(len);
    }
    Base.error("length non-string, non-list");
});
Base.apply = Base.arity((func, args) => {
    if (!Base.isPair(args) && !Base.isEmpty(args))
        Base.error("apply to non-list");
    var arr = [];
    while (Base.isPair(args)) {
        arr.push(args.first);
        args = args.rest;
    }
    return func(...arr);
});
Base.makeVariadic = Base.arity(func => {
    if (!Base.isFunc(func))
        Base.error("makeVariadic non-function");
    return (...args) => {
        return func(Base.list(...args));
    };
});
Base.show = Base.arity(v => {
    // TODO fix this
    return "" + v;
});

                               
// syntax : TODO
