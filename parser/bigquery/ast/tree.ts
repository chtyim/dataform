// generated by Textmapper; DO NOT EDIT

import { NodeType } from '../listener'
import { Selector } from '../selector'

/**
 * Tree represents an AST for some parsed content.
 */
export interface Tree {

  /**
   * Returns the content that get parsed into this tree.
   */
  text(): string;

  /**
   * Returns the root node of the tree.
   */
  root(): Node;
}

/**
 * Node represents a Node in an AST.
 */
export interface Node {

  /**
   * The {@link NodeType} represented by this node.
   */
  type(): NodeType;

  /**
   * The starting offset in the parsed content represented by this node.
   */
  offset(): number;

  /**
   * The ending offset (exclusive) in the parsed content represented by this node.
   */
  endOffset(): number;

  /**
   * The parsed content represented by this node.
   * It is essentially the tree.text().substring(offset(), endOffset()).
   */
  text(): string;

  /**
   * Returns the start position of the content of this node as 1-based line and column.
   */
  lineColumn(): { line: number; column: number };

  /**
   * Returns the next silbing node to this node that is accepted by the given {@link Selector}.
   *
   * @param selector the selector for filtering nodes
   * @returns the next silbing node accepted by the selector or null if no such node is found
   */
  next(selector: Selector): Node | null;

  /**
   * Returns the list of silibing nodes to this node that are accepted by the given {@link Selector}.
   * @param selector the selector for filtering nodes
   * @returns the list of silbing nodes that are accepted by the selector
   */
  nextAll(selector: Selector): Node[];

  /**
   * Returns the first child node to this node that is accepted by the given {@link Selector}.
   *
   * @param selector the selector for filtering nodes
   * @returns the first child node accepted by the selector or null if no such node is found
   */
  child(selector: Selector): Node | null;

  /**
   * Returns all children nodes to this node that are accepted by the given {@link Selector}.
   * @param selector the selector for filtering nodes
   * @returns the list of children nodes that are accepted by the selector
   */
  children(selector: Selector): Node[];
}
