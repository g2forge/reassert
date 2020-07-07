package com.g2forge.reassert.reassert.convert.work;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.g2forge.reassert.reassert.model.findings.UnknownWorkTypeFinding;

public class WorkModule extends SimpleModule {
	protected static abstract class UnknownWorkTypeFindingMixin {
		@JsonIgnore
		protected Throwable throwable;
	}

	private static final long serialVersionUID = -6056568239544794035L;

	@Override
	public void setupModule(SetupContext context) {
		this.setMixInAnnotation(UnknownWorkTypeFinding.class, UnknownWorkTypeFindingMixin.class);
		super.setupModule(context);
	}
}