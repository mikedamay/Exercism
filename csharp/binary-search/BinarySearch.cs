﻿using System;

// davidfrogley's solution
public static class BinarySearch
{
	public static int Find(int[] dataArg, int valueArg)
	{
		int Find(int[] data, int value, int left, int right)
		{
			var middle = (left + right) / 2;

			if (left == middle || middle == right)
			{
				return data[left] == value ? left : data[right] == value ? right : -1;
			}

			if (data[middle] == value)
			{
				return middle;
			}

			if (value < data[middle])
			{
				return Find(data, value, left, middle);
			}

			if (value > data[middle])
			{
				return Find(data, value, middle, right);
			}

			return -1;
		}
		if (dataArg.Length == 0)
		{
			return -1;
		}

		return Find(dataArg, valueArg, 0, dataArg.Length - 1);
	}


}