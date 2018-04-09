

import * as OBJ from "./obj.js";


export function decode(term) {
    var {integer, float, atom, string, list, map, tuple} = term;
    if (integer !== undefined)
        return new eInteger(term);
    if (float !== undefined)
        return new eFloat(term);
    if (atom !== undefined)
        return new eAtom(term);
    if (string !== undefined)
        return new eString(term);
    if (tuple !== undefined)
        return new eTuple(term);
    if (list !== undefined)
        return new eList(term);
    if (map !== undefined)
        return new eMap(term);
    return undefined;
}

//--------------------------------


/* Parse object: po{s, i} */
export function parse(str) {
    return _parse({s: str, i: 0});
}

function _parse(po) {
    var l = po.s.length;
    po.i = skipwsc(po.s, po.i);
    while (po.i !== l - 1) {
        var ch = po.s.charAt(po.i);
        /*
         * An atom
         */
        if (ch === "'") {
            var atom = "";
            po.i++; /* Skip the ' */
            while (po.i !== l - 1 && po.s.charAt(po.i) !== "'") {
                if (po.s.charAt(po.i) !== "'")
                    atom = atom.concat(po.s.charAt(po.i++));
            }
            po.i++; /* Skip the ' */
            return new eAtom(atom);
        }

        /*
         * A String
         */
        if (ch === '"') {
            var str = "";
            po.i++; /* Skip the " */
            while (po.i !== l - 1 && po.s.charAt(po.i) !== '"') {
                if (po.s.charAt(po.i) !== '"')
                    str = str.concat(po.s.charAt(po.i++));
            }
            po.i++; /* Skip the " */
            return new eString(str);
        }

        /*
         * A tuple
         */
        if (ch === "{") {
            po.i = skipwsc(po.s, ++po.i); /* Skip the { and any whitespaces */
            var tuples = [];
            while (po.i !== l - 1 && po.s.charAt(po.i) !== "}") {
                tuples.push(_parse(po)); /* Add element */
                po.i = skipwsc(po.s, po.i); /* Skip any whitespace */
                if (po.s.charAt(po.i) === ",")
                    po.i++; /* Skip any comma */
            }
            po.i++; /* Skip the } */
            return new eTuple(tuples);
        }

        /*
         * A list
         */
        if (ch === "[") {
            po.i = skipwsc(po.s, ++po.i); /* Skip the [ and any whitespaces */
            var list = [];
            while (po.i !== l - 1 && po.s.charAt(po.i) !== "]") {
                list.push(_parse(po)); /* Add element */
                po.i = skipwsc(po.s, po.i); /* Skip any whitespace */
                if (po.s.charAt(po.i) === ",")
                    po.i++; /* Skip any comma */
            }
            po.i++; /* Skip the ] */
            return new eList(list);
        }

        /*
         * A map
         */
        if (ch === "#" && po.s.charAt(po.i + 1) === "{") {
            po.i = skipwsc(po.s, po.i + 2); /* Skip the #{ and any whitespaces */
            var map = [];
            while (po.i !== l - 1 && po.s.charAt(po.i) !== "}") {
                var kv = {};
                kv['key'] = _parse(po); /* Add key */
                po.i = skipwsc(po.s, po.i); /* Skip any whitspace */
                if (!(po.s.charAt(po.i) === "=" && po.s.charAt(po.i + 1) === ">"))
                    throw "Not a map";
                po.i = skipwsc(po.s, po.i + 2); /* Skip => and any whitespace */
                kv['value'] = _parse(po); /* Add value */
                map.push(kv);
                if (po.s.charAt(po.i) === ",")
                    po.i++; /* Skip any comma */
            }
            po.i++; /* Skip the } */
            return new eMap(map);
        }



    }



}

/* We don't assume any comments (%) here */
function skipwsc(string, i) {
    do {
        var ch = string.charAt(i);
        if (!isspace(ch))
            return i;
    } while (++i !== string.length - 1)

    return i;
}

function isspace(c) {
    return (c === " ") || (c === "\t") || (c === "\n");
}


//---------------------------------





export class eBase {
    constructor(type, value) {
        this._type = type;
        this._value = value;
    }
    type() {
        return this._type;
    }
    value() {
        return this._value;
    }
}

/*
 * Construct:
 *      var integer = new eInteger(42);
 * Deconstruct:
 *      var a = integer.value(); 
 */
export class eInteger extends eBase {
    constructor(value) {
        if (typeof (value) === "number") {
            super("integer", value);
        } else if (value instanceof eInteger) {
            super("integer", value.value());
        } else if (value instanceof Object && value.integer !== undefined) {
            super("integer", value.integer);
        } else
            throw "Not an 'number', eInteger or a Carolus Domi Intermediate format";
    }
    encode() {
        return {integer: this._value};
    }
    toString() {
        return this._value.toString();
    }
}

/*
 * Construct:
 *      var f = new eFloat(42);
 * Deconstruct:
 *      var a = f.value(); 
 */
export class eFloat extends eBase {
    constructor(value) {
        if (typeof (value) === "number") {
            super("float", value);
        } else if (value instanceof eFloat) {
            super("float", value.value());
        } else if (value instanceof Object && value.float !== undefined) {
            super("atom", value.float);
        } else
            throw "Not an 'number', eFloat or a Carolus Domi Intermediate format";
    }
    encode() {
        return {float: this._value};
    }
    toString() {
        return this._value.toString();
    }
}



/*
 * Construct:
 *      var atom = new eAtom("a");
 * Deconstruct:
 *      var a = atom.value(); 
 */

export class eAtom extends eBase {
    constructor(value) {
        if (typeof (value) === "string") {
            super("atom", value);
        } else if (value instanceof eAtom) {
            super("atom", value.value());
        } else if (value instanceof Object && value.atom !== undefined) {
            super("atom", value.atom);
        } else
            throw "Not an 'string', eString or a Carolus Domi Intermediate format";
    }
    encode() {
        return {atom: this._value};
    }
    toString() {
        return "'" + this._value + "'";
    }
}


/*
 * Construct:
 *      var string = new eString("a");
 * Deconstruct:
 *      var a = string.value(); 
 */
export class eString extends eBase {
    constructor(value) {
        if (typeof (value) === "string") {
            super("string", value);
        } else if (value instanceof eAtom) {
            super("string", value.value());
        } else if (value instanceof Object && value.string !== undefined) {
            super("string", value.string);
        } else
            throw "Not an 'string', eString or a Carolus Domi Intermediate format";
    }
    encode()
    {
        return {string: this._value};
    }
    toString() {
        return '"' + this._value + '"';
    }
    forEach() {
        /* 
         * All Erlang strings are array of characters. An empty list can
         * therefore be encode as an empty string by the server.
         * Handle this here.
         */

        if (this._value === "") {
            /* Empty list, nothing to iterate */
        }
    }
    lLength() {
        /* 
         * All Erlang strings are array of characters. An empty list can
         * therefore be encode as an empty string by the server.
         * Handle this here.
         */

        if (this._value === "") {
            return 0;
        }
    }
}

/*
 * Construct:
 *      var tuple = new eTuple([new eAtom("a"), new eAtom("b"), new eString("c")]);
 * Deconstruct:
 *      var [a, b, c] = tuple.value(); 
 *      
 */
export class eTuple extends eBase {
    constructor(value) {
        if (value instanceof Array) {
            super("tuple", value);
        } else if (value instanceof eTuple) {
            super("tuple", value.value());
        } else if (value instanceof Object && value.tuple !== undefined) {
            var list = value.tuple;
            super("tuple", []);
            var l = list.length;
            for (var i = 0; i !== l; i++) {
                this._value.push(decode(list[i]));
            }
        } else
            throw "Not an Array, eTuple or a Carolus Domi Intermediate format";
    }
    encode() {
        var eObj = [];
        this._value.forEach(function (e) {
            eObj.push(e.encode());
        });
        return {tuple: eObj};
    }
    toString() {
        var eStr = "";
        var l = this._value.length - 1;
        this._value.forEach(function (e, i) {
            eStr += e.toString();
            if (i !== l)
                eStr += ",";
        });
        return "{" + eStr + "}";
    }
}


/*
 * Construct:
 *      var list = new eList([new eAtom("a"), new eAtom("b"), new eString("c")]);
 * Deconstruct:
 *      var [a, b, c] = list.value(); 
 *      
 *  list.indexOf(new eAtom("b"));    
 */
export class eList extends eBase {
    constructor(value) {
        if (value instanceof Array) {
            super("list", value);
        } else if (value instanceof eList) {
            super("list", value.value());
        } else if (value instanceof Object && value.list !== undefined) {
            var list = value.list;
            super("list", []);
            var l = list.length;
            for (var i = 0; i !== l; i++) {
                this._value.push(decode(list[i]));
            }
        } else
            throw "Not an Array, eList or a Carolus Domi Intermediate format";
    }
    encode() {
        var eObj = [];
        this._value.forEach(function (e) {
            eObj.push(e.encode());
        });
        return {list: eObj};
    }
    toString() {
        var eStr = "";
        var l = this._value.length - 1;
        this._value.forEach(function (e, i) {
            eStr += e.toString();
            if (i !== l)
                eStr += ",";
        });
        return "[" + eStr + "]";
    }
    indexOf(eterm) {
        var l = this._value.length;
        for (var i = 0; i !== l; i++) {
            if (OBJ.OBJ.compare(this._value[i], eterm)) {
                return i;
            }
        }
        /* Value not found */
        return -1;
    }
    /*
     * Helper for finding atoms by creating an atom
     */
    indexOfAtom(str) {
        return this.indexOf(new eAtom(str));
    }
    forEach(fun, this_) {
        this._value.forEach(fun, this_);
    }
    lLength() {
        return this._value.length;
    }
}

/*
 * Construct:
 *      var map = new eMap([{key: new eAtom("a"), value: new eAtom("b")}, {key:new eString("c"), value:new eTuple([])}]);
 * Deconstruct:
 *      var a = map.value(new eAtom("a")); 
 *      
 *  map.keys();
 */
/* {'map: [{'key': {'atom': "a"}, 'value': {'atom': "map"}}]} */

export class eMap extends eBase {
    constructor(value) {
        super("map", undefined);
        if (value instanceof Array) {
            this._keys = [];
            this._values = [];
            /* Extract keys and value */
            var l = value.length;
            for (var i = 0; i !== l; i++) {
                this._keys.push(value[i].key);
                this._values.push(value[i].value);
            }
        } else if (value instanceof eMap) {
            this._keys = value._keys;
            this._values = value._values;
        } else if (value instanceof Object && value.map !== undefined) {
            var list = value.map;
            this._keys = [];
            this._values = [];
            var l = list.length;
            for (var i = 0; i !== l; i++) {
                this._keys.push(decode(list[i].key));
                this._values.push(decode(list[i].value));
            }
        } else
            throw "Not an Array, eMap or a Carolus Domi Intermediate format";
    }
    value(eterm) {
        var l = this._keys.length;
        for (var i = 0; i !== l; i++) {
            if (OBJ.OBJ.compare(this._keys[i], eterm)) {
                return this._values[i];
            }
        }
        /* Key not found */
        return undefined;
    }
    /* Convienience fuction for an atom based key */
    valueAtom(str) {
        return this.value(new eAtom(str));
    }
    encode() {
        var eObj = [];
        var l = this._keys.length;
        for (var i = 0; i !== l; i++) {
            eObj.push({
                key: this._keys[i].encode(),
                value: this._values[i].encode()
            });
        }
        return {map: eObj};
    }
    toString() {
        var eStr = "";
        var l = this._keys.length;
        for (var i = 0; i !== l; i++) {
            eStr += this._keys[i].toString();
            eStr += "=>";
            eStr += this._values[i].toString();
            if (i !== l - 1)
                eStr += ",";
        }
        return "#{" + eStr + "}";
    }
    keys() {
        return this._keys;
    }
}

