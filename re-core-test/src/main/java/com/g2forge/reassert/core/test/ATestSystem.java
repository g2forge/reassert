package com.g2forge.reassert.core.test;

import com.g2forge.reassert.contract.StandardModule;
import com.g2forge.reassert.core.api.module.Context;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.api.module.core.CoreModule;
import com.g2forge.reassert.core.api.system.ISystem;
import com.g2forge.reassert.mock.MockModule;

import lombok.AccessLevel;
import lombok.Getter;

public abstract class ATestSystem {
	@Getter(value = AccessLevel.PROTECTED, lazy = true)
	private final IContext context = computeContext(Context.builder()).build();

	public Context.ContextBuilder computeContext(Context.ContextBuilder builder) {
		return builder.module(CoreModule.create()).module(MockModule.create()).module(StandardModule.create());
	}

	protected abstract ISystem<?> getSystem();
}
