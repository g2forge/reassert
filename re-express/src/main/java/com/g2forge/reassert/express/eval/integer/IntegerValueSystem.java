package com.g2forge.reassert.express.eval.integer;

import java.util.Objects;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.enigma.backend.convert.textual.ITextualRenderer;
import com.g2forge.enigma.backend.convert.textual.ToStringTextualRenderer;
import com.g2forge.reassert.express.eval.value.IValueSystem;
import com.g2forge.reassert.express.eval.value.ValueValidation;

public class IntegerValueSystem implements IValueSystem<Integer>, ISingleton {
	private static final IntegerValueSystem INSTANCE = new IntegerValueSystem();

	public static IntegerValueSystem create() {
		return INSTANCE;
	}

	protected IntegerValueSystem() {}

	@Override
	public ITextualRenderer<? super Integer> getRenderer() {
		return ToStringTextualRenderer.create();
	}

	@Override
	public boolean isEqual(Integer left, Integer right) {
		return Objects.equals(left, right);
	}

	@Override
	public boolean isSame(Integer left, Integer right) {
		return Objects.equals(left, right);
	}

	@Override
	public IValidation isValid(Integer value) {
		return new ValueValidation<>(value, value != null);
	}
}
