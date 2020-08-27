package com.g2forge.reassert.core.api.module.core;

import com.g2forge.alexandria.annotations.service.Service;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.api.module.IModule;
import com.g2forge.reassert.core.api.module.IModule.Loaded.LoadedBuilder;

@Service(IModule.class)
public class CoreModule implements IModule, ISingleton {
	protected static final CoreModule INSTANCE = new CoreModule();

	public static CoreModule create() {
		return INSTANCE;
	}

	private CoreModule() {}

	@Override
	public Loaded load(IContext context) {
		final LoadedBuilder builder = IModule.Loaded.builder();
		builder.describer(ArtifactDescriber.create());
		builder.describer(FileDescriber.create());
		builder.describer(UnknownLicenseDescriber.create());
		builder.describer(UnspecifiedLicenseDescriber.create());
		builder.describer(UnspecifiedUsageDescriber.create());
		builder.describer(StringNamedDescriber.create());
		return builder.build();
	}
}
