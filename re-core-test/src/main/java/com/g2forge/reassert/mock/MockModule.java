package com.g2forge.reassert.mock;

import com.g2forge.alexandria.annotations.service.Service;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.api.module.IModule;

@Service(IModule.class)
public class MockModule implements IModule, ISingleton {
	protected static final MockModule INSTANCE = new MockModule();

	public static MockModule create() {
		return INSTANCE;
	}

	private MockModule() {}

	@Override
	public Loaded load(IContext context) {
		return IModule.Loaded.builder().system(MockSystem.create()).describer(MockCoordinatesDescriber.create()).build();
	}
}
