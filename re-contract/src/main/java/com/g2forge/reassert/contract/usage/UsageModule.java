package com.g2forge.reassert.contract.usage;

import com.g2forge.alexandria.annotations.service.Service;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.api.module.IModule;
import com.g2forge.reassert.core.api.module.IModule.Loaded.LoadedBuilder;

@Service(IModule.class)
public class UsageModule implements IModule, ISingleton {
	protected static final UsageModule INSTANCE = new UsageModule();

	public static UsageModule create() {
		return INSTANCE;
	}

	private UsageModule() {}

	@Override
	public Loaded load(IContext context) {
		final LoadedBuilder builder = IModule.Loaded.builder();
		builder.describer(StandardUsageDescriber.create());
		return builder.build();
	}
}
