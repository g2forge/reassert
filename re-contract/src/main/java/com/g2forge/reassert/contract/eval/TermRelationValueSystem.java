package com.g2forge.reassert.contract.eval;

import java.util.Objects;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.enigma.backend.convert.textual.ITextualRenderer;
import com.g2forge.enigma.backend.convert.textual.ToStringTextualRenderer;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.eval.value.IValueSystem;
import com.g2forge.reassert.express.eval.value.ValueValidation;

public class TermRelationValueSystem implements IValueSystem<TermRelation>, ISingleton {
	private static final TermRelationValueSystem INSTANCE = new TermRelationValueSystem();

	public static TermRelationValueSystem create() {
		return INSTANCE;
	}

	protected TermRelationValueSystem() {}

	@Override
	public ITextualRenderer<? super TermRelation> getRenderer() {
		return ToStringTextualRenderer.create();
	}

	@Override
	public boolean isEqual(TermRelation left, TermRelation right) {
		return Objects.equals(left, right);
	}

	@Override
	public boolean isSame(TermRelation left, TermRelation right) {
		return Objects.equals(left, right);
	}

	@Override
	public IValidation isValid(TermRelation value) {
		return new ValueValidation<>(value, value != null);
	}
}
