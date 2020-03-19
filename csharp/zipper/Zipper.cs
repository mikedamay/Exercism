using System;
using System.Collections.Generic;
using System.Reflection.Metadata.Ecma335;

public class BinTree
{
    public BinTree(int value, BinTree left, BinTree right)
    {
        (Value, Left, Right) = (value, left, right);
    }

    public int Value { get; }
    public BinTree Left { get; }
    public BinTree Right { get; }
    public override bool Equals(object other)
    {
        if (other == null) return false;
        if (ReferenceEquals(this, other)) return true;
        if (other.GetType() != this.GetType()) return false;
        return Equals((BinTree)other);
    }

    protected bool Equals(BinTree other)
    {
        if (other == null) return false;
        return this.Value == other.Value 
          && (this.Left == null && other.Left == null
          || this.Left != null && this.Left.Equals(other.Left))
          && (this.Right == null && other.Right == null
          || this.Right != null && this.Right.Equals(other.Right));
    }

    public override int GetHashCode()
    {
        return (this.Value 
          + (this.Left == null ? 0 : this.Left.GetHashCode()) 
          + (this.Right == null ? 0 : this.Right.GetHashCode())) % 397;
    }
}

public class Zipper
{   
    private Stack<BinTree> breadcrumbs = new Stack<BinTree>();

    public Zipper(BinTree binTree)
    {
        breadcrumbs.Push(binTree);
    }
    public int Value()
    {
        throw new NotImplementedException("You need to implement this function.");
    }

    public Zipper SetValue(int newValue)
    {
        throw new NotImplementedException("You need to implement this function.");
    }

    public Zipper SetLeft(BinTree binTree)
    {
        throw new NotImplementedException("You need to implement this function.");
    }

    public Zipper SetRight(BinTree binTree) 
    {
        throw new NotImplementedException("You need to implement this function.");
    }

    public Zipper Left()
    {
        throw new NotImplementedException("You need to implement this function.");
    }

    public Zipper Right()
    {
        throw new NotImplementedException("You need to implement this function.");
    }

    public Zipper Up()
    {
        throw new NotImplementedException("You need to implement this function.");
    }

    public BinTree ToTree()
    {
        return breadcrumbs.ToArray()[breadcrumbs.Count - 1];
    }

    public static Zipper FromTree(BinTree tree)
    {
        return new Zipper(tree);
    }

}