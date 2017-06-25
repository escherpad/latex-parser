"use strict";
/**
 * @fileoverview General JavaScript utils
 * This file is a part of TeXnous project.
 *
 * @copyright TeXnous project team (http://texnous.org) 2016
 * @license LGPL-3.0
 *
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version 3
 * of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this library;
 * if not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */
Object.defineProperty(exports, "__esModule", { value: true });
/**
 * Update object properties by property values
 * @param {!Object} target the object to copy properties to
 * @param {!Object} values the object with property values (undefined values will be skipped)
 * @param {(!Object.<string,string>|!Array.<string>)=} opt_keys
 *        list of keys or map of the target keys to the property names, all the enumerable
 *        properties will be used if undefined
 * @param {{writable:boolean,enumerable:boolean,configurable:boolean}=} opt_attributes
 *        property attributes, { writable: true, enumerable: true, configurable: true } by default
 */
function updateProperties(target, values, opt_keys, opt_attributes) {
    if (opt_attributes === void 0) { opt_attributes = {
        writable: true,
        enumerable: true,
        configurable: true
    }; }
    if (!(target instanceof Object))
        throw new TypeError('"target" isn\'t an Object instance');
    if (values === undefined)
        return; // do noting is the sources is undefined
    if (!(values instanceof Object))
        throw new TypeError('"properties" isn\'t an Object instance');
    if (opt_attributes === undefined) {
        opt_attributes = { writable: true, enumerable: true, configurable: true };
    }
    else if (!(opt_attributes instanceof Object)) {
        throw new TypeError('"attributes" isn\'t an Object instance');
    }
    if (opt_keys === undefined) {
        for (var key in values) {
            //noinspection JSUnfilteredForInLoop
            if (values[key] !== undefined) {
                //noinspection JSUnfilteredForInLoop
                Object.defineProperty(target, key, // update the property
                // using the defined value
                Object.create(opt_attributes, { value: { value: values[key] } }));
            }
        }
    }
    else if (opt_keys instanceof Array) {
        opt_keys.forEach(function (key) {
            if (values[key] !== undefined) {
                Object.defineProperty(target, key, // update the property
                Object.create(opt_attributes, { value: { value: values[key] } }) // using the defined value
                );
            }
        });
    }
    else if (opt_keys instanceof Object) {
        for (var targetKey in opt_keys) {
            //noinspection JSUnfilteredForInLoop
            var key = opt_keys[targetKey]; // the sources key
            if (values[key] !== undefined)
                //noinspection JSUnfilteredForInLoop
                Object.defineProperty(target, targetKey, // update the property
                // using the defined value
                Object.create(opt_attributes, { value: { value: values[key] } }));
        }
    }
    else {
        throw new TypeError('"keys" isn\'t an Object instance');
    }
}
exports.updateProperties = updateProperties;
/**
 * Test object properties with property values (strict comparing is used)
 * @param {!Object} target the object with properties to test
 * @param {?Object} values the object with property values (undefined values will be skipped)
 * @param {?(Object.<string,string>|Array.<string>)} opt_keys
 *        list of keys or map of the target keys to the property names, all the enumerable
 *        properties will be used if undefined
 * @param {boolean=true} opt_skipUndefined true to skip keys with undefined values, false otherwise
 * @return {boolean} true if all the defined properties are the same false otherwise
 */
function testProperties(target, values, opt_keys, opt_skipUndefined) {
    if (opt_skipUndefined === void 0) { opt_skipUndefined = true; }
    if (!(target instanceof Object))
        throw new TypeError('"target" isn\'t an Object instance');
    if (values === undefined)
        return true; // do noting is the sources is undefined
    if (!(values instanceof Object))
        throw new TypeError('"properties" isn\'t an Object instance');
    if (opt_skipUndefined === undefined)
        opt_skipUndefined = true; // skip undefined by default
    if (opt_keys === undefined) {
        for (var key in values) {
            //noinspection JSUnfilteredForInLoop // TODO what to do?
            if (target[key] !== values[key]
                && !(values[key] === undefined && opt_skipUndefined))
                return false; // false if any value is different
        }
    }
    else if (opt_keys instanceof Array) {
        return opt_keys.every(function (key) {
            return target[key] === values[key] || (values[key] === undefined && opt_skipUndefined);
        });
    }
    else if (opt_keys instanceof Object) {
        for (var targetKey in opt_keys) {
            var key = opt_keys[targetKey]; // the sources key
            if (target[targetKey] !== values[key] && !(values[key] === undefined && opt_skipUndefined))
                return false; // false if any value is different
        }
    }
    else {
        throw new TypeError('"keys" isn\'t an Object instance');
    }
    return true; // return true if all the defined properties are the same
}
exports.testProperties = testProperties;
function isNumber(x) {
    return typeof x === "number";
}
exports.isNumber = isNumber;
function isString(x) {
    return typeof x === "string";
}
exports.isString = isString;
function mustNotBeUndefined(x, msg) {
    if (!x)
        throw new Error(msg);
    return x;
}
exports.mustNotBeUndefined = mustNotBeUndefined;
// @Deprecated
function mustBeObject(o, msg) {
    if (!(o instanceof Object))
        throw new TypeError(msg ? msg : "Expected Object");
    return o;
}
exports.mustBeObject = mustBeObject;
function mustBeString(o, msg) {
    if (typeof o !== "string")
        throw new TypeError(msg ? msg : "Expected string");
    return o;
}
exports.mustBeString = mustBeString;
function mustBeArray(a, msg) {
    if (!(isArray(a)))
        throw new TypeError(msg ? msg : "Expected Array");
    return a;
}
exports.mustBeArray = mustBeArray;
//noinspection JSUnusedGlobalSymbols
function isArray(x) {
    return x.constructor === Array;
}
exports.isArray = isArray;
exports.mconcat = function (mappend) {
    var args = [];
    for (var _i = 1; _i < arguments.length; _i++) {
        args[_i - 1] = arguments[_i];
    }
    return args.reduceRight(mappend);
};
exports.snd = function (pair) { return pair[1]; };
function concatMap(arr, f) {
    return [].concat.apply([], arr.map(f));
}
exports.concatMap = concatMap;
