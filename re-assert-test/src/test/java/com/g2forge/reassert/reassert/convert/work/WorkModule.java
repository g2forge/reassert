package com.g2forge.reassert.reassert.convert.work;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.g2forge.reassert.contract.algorithm.worklicense.model.finding.UnknownWorkLicenseRulesFinding;

public class WorkModule extends SimpleModule {
	protected static abstract class UnknownLicenseRulesFindingMixin {
		@JsonIgnore
		protected Throwable throwable;
	}

	private static final long serialVersionUID = -6056568239544794035L;

	@Override
	public void setupModule(SetupContext context) {
		setMixInAnnotation(UnknownWorkLicenseRulesFinding.class, UnknownLicenseRulesFindingMixin.class);
		super.setupModule(context);
	}
}