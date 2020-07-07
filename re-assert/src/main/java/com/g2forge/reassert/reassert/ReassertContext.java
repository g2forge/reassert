package com.g2forge.reassert.reassert;

import com.g2forge.alexandria.service.BasicServiceLoader;
import com.g2forge.alexandria.service.DefaultInstantiator;
import com.g2forge.reassert.core.api.module.Context;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.api.module.IModule;

import lombok.Getter;

public class ReassertContext {
	@Getter(lazy = true)
	private static final IContext context = new Context(new BasicServiceLoader<>(null, IModule.class, null, new DefaultInstantiator<>(null, IModule.class)).load());
}