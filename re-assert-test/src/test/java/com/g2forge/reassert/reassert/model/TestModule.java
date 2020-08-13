package com.g2forge.reassert.reassert.model;

import com.g2forge.alexandria.annotations.service.Service;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.api.module.IModule;
import com.g2forge.reassert.core.api.module.IModule.Loaded.LoadedBuilder;
import com.g2forge.reassert.reassert.model.contract.TestLicenseDescriber;
import com.g2forge.reassert.reassert.model.contract.TestUsageDescriber;

@Service(IModule.class)
public class TestModule implements IModule, ISingleton {
	protected static final TestModule INSTANCE = new TestModule();

	public static TestModule create() {
		return INSTANCE;
	}

	private TestModule() {}

	@Override
	public Loaded load(IContext context) {
		final LoadedBuilder builder = IModule.Loaded.builder();
		builder.describer(TestLicenseDescriber.create());
		builder.describer(TestUsageDescriber.create());
		return builder.build();
	}
}
