package com.g2forge.reassert.express.eval.bool;

import java.util.Objects;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.enigma.backend.convert.textual.ITextualRenderer;
import com.g2forge.enigma.backend.convert.textual.ToStringTextualRenderer;
import com.g2forge.reassert.express.eval.value.IValueSystem;
import com.g2forge.reassert.express.eval.value.ValueValidation;

public class BooleanValueSystem implements IValueSystem<Boolean>, ISingleton {
	private static final BooleanValueSystem INSTANCE = new BooleanValueSystem();

	public static BooleanValueSystem create() {
		return INSTANCE;
	}

	protected BooleanValueSystem() {}

	@Override
	public ITextualRenderer<? super Boolean> getRenderer() {
		return ToStringTextualRenderer.create();
	}

	@Override
	public boolean isEqual(Boolean left, Boolean right) {
		return Objects.equals(left, right);
	}

	@Override
	public boolean isSame(Boolean left, Boolean right) {
		return Objects.equals(left, right);
	}

	@Override
	public IValidation isValid(Boolean value) {
		return new ValueValidation<>(value, value != null);
	}
}
