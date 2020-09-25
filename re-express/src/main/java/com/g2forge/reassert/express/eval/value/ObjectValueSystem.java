package com.g2forge.reassert.express.eval.value;

import java.util.Objects;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.enigma.backend.convert.textual.ITextualRenderer;
import com.g2forge.enigma.backend.convert.textual.ToStringTextualRenderer;

public class ObjectValueSystem implements IValueSystem<Object>, ISingleton {
	private static final ObjectValueSystem INSTANCE = new ObjectValueSystem();

	public static ObjectValueSystem create() {
		return INSTANCE;
	}

	protected ObjectValueSystem() {}

	@Override
	public ITextualRenderer<? super Object> getRenderer() {
		return ToStringTextualRenderer.create();
	}

	@Override
	public boolean isEqual(Object left, Object right) {
		return Objects.equals(left, right);
	}

	@Override
	public IValidation isValid(Object value) {
		return new ValueValidation<>(value, value != null);
	}
}
