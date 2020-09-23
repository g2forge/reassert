package com.g2forge.reassert.list.convert;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.g2forge.alexandria.java.fluent.optional.IOptional;

public class ExpressionModule extends SimpleModule {
	protected static abstract class IOptionalMixin {
		@JsonIgnore
		public abstract boolean isNotEmpty();
	}

	private static final long serialVersionUID = -87305109666615339L;

	@Override
	public void setupModule(SetupContext context) {
		setMixInAnnotation(IOptional.class, IOptionalMixin.class);
		super.setupModule(context);
	}
}