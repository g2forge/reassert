package com.g2forge.reassert.express.v2.eval.integer;

import java.util.Objects;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.express.v2.eval.value.IValueSystem;

public class IntegerValueSystem implements IValueSystem<Integer>, ISingleton {
	private static final IntegerValueSystem INSTANCE = new IntegerValueSystem();

	public static IntegerValueSystem create() {
		return INSTANCE;
	}

	protected IntegerValueSystem() {}

	@Override
	public boolean isEqual(Integer left, Integer right) {
		return Objects.equals(left, right);
	}

	@Override
	public boolean isSame(Integer left, Integer right) {
		return Objects.equals(left, right);
	}

	@Override
	public boolean isValid(Integer value) {
		return value != null;
	}
}
