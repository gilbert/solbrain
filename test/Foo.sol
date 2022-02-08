// SPDX-License-Identifier: MIT
// pragma solidity ^0.8.0;

contract GreeterSimple {
    // bytes32 private greeting = "Hello, EVM!";

    // function greet() public view returns (bytes32) {
    //     return greeting;
    // }

    // function setGreeting(bytes32 _greeting) external {
    function setGreeting(bytes32 _greeting) {
        greeting = _greeting;
    }
}
